(ns specy.event
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]
            [tick.alpha.api :as t]
            [specy.uuid :as uuid]
            [specy.utils :refer [create-map]]
            [specy.time :as time]
            [specy.command :as command]
            [specy.query :as query]
            [specy.protocols :refer :all]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.utils :refer [inspect operations parse-opts+specs]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Spec                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::event-type keyword?)
(s/def ::event-id uuid?)
(s/def ::from-command-ref uuid?)
(s/def ::from-query-ref uuid?)
(s/def ::published-at ::time/instant)
(s/def ::published-by string?)
(s/def ::from-command ::command/command-metadata)
(s/def ::from-query ::query/query-metadata)
(s/def ::org-ref string?)
(s/def ::correlation-id uuid?)
(s/def ::container string?)
(s/def ::source string?)

(defmulti event-type :event-type)
(s/def ::event (s/multi-spec event-type :event-type))

(s/def ::event-metadata (s/keys :req-un [::event-type ::event-id ::org-ref ::correlation-id ::published-at ::published-by]
                                :opt-un [::container ::source ::from-command-ref ::from-query-ref ::from-command ::from-query]))

(defn event-meta-gen
  "event metadata generator with fixed event-type"
  [event-type]
  (check-gen/let [event-type (check-gen/return event-type)
                  event-id (s/gen ::event-id)
                  correlation-id (s/gen ::correlation-id)
                  org-ref (s/gen ::org-ref)
                  published-at (check-gen/one-of [(check-gen/return (t/now)) (s/gen ::published-at)])
                  published-by (s/gen ::published-by)
                  container (s/gen ::container)
                  source (s/gen ::source)
                  from-command-ref (s/gen ::from-command-ref)
                  from-query-ref (s/gen ::from-query-ref)]
                 (create-map event-type event-id correlation-id org-ref published-at published-by container source from-command-ref)))

(defn event-metadata
  [{:keys [event-id event-type published-by published-at org-ref container source correlation-id from-command-ref from-query-ref from-command from-query] :as metadata}]
  (let [from-command-ref (or from-command-ref (:command-id metadata))
        from-query-ref (or from-query-ref (:query-id metadata))]
    (merge {:event-id       (or event-id (uuid/random))
            :published-at   (t/instant)
            :published-by   (or published-by (or (:issued-by from-command) (:issued-by from-query) (:issued-by metadata))) ;;in case of cmd or query we take the issued-by ;to delete after refactor
            :correlation-id (or correlation-id (uuid/random))} ;to delete after refactor
           (select-keys from-command [:org-ref :correlation-id :source :container])
           (select-keys from-query [:org-ref :correlation-id :source :container])
           metadata)))

(defmacro defevent
  "define a event with first arg the ns keyword of the event then the ns keyword of the spec"
  [evtk datak & options]
  (let [options (first options)
        ns *ns*]
    `(do
       (s/def ~evtk
         (s/with-gen (s/merge ::event-metadata (s/keys :req-un [~datak]))
                     #(gen/fmap (fn [[event-meta# org#]] (assoc event-meta# (keyword (name ~datak)) org#))
                                (gen/tuple (event-meta-gen ~evtk) (s/gen ~datak)))))

       (defmethod event-type ~evtk [_#] (s/get-spec ~evtk))

       (defn ~(-> (symbol (str "->" (name evtk)))
                  (vary-meta assoc :domain-asset {:name evtk
                                                  :type :query
                                                  :spec evtk}))
         ~(str "Create an event of type " evtk " with metadata (complementing the missing optional data, only :org-ref and :published-by are mandatory keys in metadata) and entity " datak)
         [metadata# entity#]
         (let [merged-metadata# (event-metadata (assoc metadata# :event-type ~evtk))]
           (s/assert* ::event-metadata merged-metadata#)
           (s/assert* ~datak entity#)
           (assoc merged-metadata# :entity entity#)))

       (defn ~(symbol (str (name evtk) "-gen")) ~(str "Return a generator for the event type " evtk " and entity " datak) []
         (check-gen/let [meta# (event-meta-gen ~evtk)
                         entity# (s/gen ~datak)]
                        (assoc meta# :entity entity#)))

       (let [event-desc# {:name     ~(name evtk)
                          :longname (clojure.reflect/typename (symbol ~evtk))
                          :ns       ~ns                     ;;caller ns
                          :id       ~evtk
                          :kind     :event
                          :spec     ~evtk
                          :rely-on  ~(get options :rely-on)
                          :doc      ~(:doc options)}]
         (store! building-blocks event-desc#)
         (publish! bus event-desc#)
         event-desc#))))

(defn events "return all the events declared with defevent that are in scope" []
  (-> event-type methods keys set))

