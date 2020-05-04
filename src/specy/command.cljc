(ns specy.command
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]
            [tick.alpha.api :as t]
            [specy.uuid :as uuid]
            [specy.utils :refer [create-map]]
            [specy.time :as time]
            [specy.protocols :refer :all]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.utils :refer [inspect operations parse-opts+specs]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command Spec                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti command-type :command-type)
(s/def ::comand (s/multi-spec command-type :command-type))

;;command are implemented in domain project like:
;;  (defmethod command-type ::create-organization-command [_]
;;    (s/keys :req-un [::organization ::command]))
;; ::command spec took one of the command define with the multimethod

(s/def ::command-type keyword?)
(s/def ::command-id uuid?)
(s/def ::correlation-id uuid?)
(s/def ::org-ref string?)
(s/def ::issued-at ::time/instant)
(s/def ::issued-by string?)
(s/def ::received-at ::time/instant)
(s/def ::source string?)
(s/def ::container string?)

(s/def ::command-metadata (s/keys :req-un [::command-type ::command-id ::org-ref ::issued-at ::issued-by]
                                  :opt-un [::source ::container]))

(defn command-meta-gen
  "command metadata generator with fixed command-type"
  [command-type]
  (check-gen/let [command-type (check-gen/return command-type)
                  command-id (s/gen ::command-id)
                  org-ref (s/gen ::org-ref)
                  issued-at (check-gen/one-of [(check-gen/return (t/now)) (s/gen ::issued-at)])
                  issued-by (s/gen ::issued-by)
                  source (s/gen ::source)
                  container (s/gen ::container)]
                 (create-map command-type command-id org-ref issued-at issued-by source container)))

(defn command-metadata [{:keys [command-id command-type issued-at correlation-id] :as metadata}]
  (merge {:command-id     (or command-id (uuid/random))
          :issued-at      (or issued-at (t/instant))
          :correlation-id (or correlation-id (uuid/random))} metadata))

(defmacro defcommand
  "define a command with first arg the ns keyword of the command then the ns keyword of the spec"
  [cmdk datak & options]
  (let [options (first options)
        ns *ns*]
    `(do (s/def ~cmdk
           (s/with-gen (s/merge ::command-metadata (s/keys :req-un [~datak]))
                       #(gen/fmap (fn [[command-meta# data#]]
                                    (assoc command-meta# (keyword (name ~datak)) data#))
                                  (gen/tuple (command-meta-gen ~cmdk) (s/gen ~datak)))))
         (defmethod command-type ~cmdk [_#] (s/get-spec ~cmdk))

         (defn ~(symbol (str "->" (name cmdk)))
           ~(str "Create a command of type " cmdk " with metadata (complementing the missing optional data, only :org-ref and :issued-by are mandatory keys in metadata) and data " datak) [metadata# data#]
           (let [merged-metadata# (command-metadata (assoc metadata# :command-type ~cmdk))]
             (s/assert* ::command-metadata merged-metadata#)
             (s/assert* ~datak data#)
             (assoc merged-metadata# (keyword (name ~datak)) data#)))

         (defn ~(symbol (str (name cmdk) "-gen")) ~(str "Return a generator for the command type " cmdk " and data " datak) []
           (check-gen/let [meta# (command-meta-gen ~cmdk)
                           data# (s/gen ~datak)]
                          (assoc meta# (keyword (name ~datak)) data#)))

         (let [command-desc# {:name     ~(name cmdk)
                            :longname (clojure.reflect/typename (symbol ~cmdk))
                            :ns       ~ns                   ;;caller ns
                            :id       ~cmdk
                            :kind     :command
                            :spec     ~cmdk
                            :rely-on  ~(get options :rely-on)
                            :doc      ~(:doc options)}]
           (store! building-blocks command-desc#)
           (publish! bus command-desc#)
           command-desc#))))

(defn commands "return all the commands declared in scope" []
  (-> command-type methods keys set))


