(ns specy.query
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]
            [tick.alpha.api :as t]
            [specy.time :as time]
            [specy.uuid :as uuid]
            [specy.utils :refer [create-map]]
            [specy.protocols :refer :all]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.utils :refer [inspect operations parse-opts+specs]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query Spec                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::query-type keyword?)
(s/def ::query-id uuid?)
(s/def ::query any?)
(s/def ::org-ref string?)
(s/def ::issued-at ::time/instant)
(s/def ::issued-by string?)
(s/def ::source string?)
(s/def ::received-at ::time/instant)

(defmulti query-type :query-type)

(s/def ::query-metadata (s/keys :req-un [::query-type ::query-id ::org-ref ::issued-at ::issued-by]
                                :opt-un [::query ::source ::received-at]))

(defn query-metadata [{:keys [query-id query-type issued-at correlation-id] :as metadata}]
  (merge {:query-id       (or query-id (uuid/random))
          :issued-at      (or issued-at (t/instant))
          :correlation-id (or correlation-id (uuid/random))} metadata))

(defn query-meta-gen
      "query metadata generator with fixed command-type"
      [query-type]
      (check-gen/let [query-type (check-gen/return query-type)
                      query-id (s/gen ::query-id)
                      org-ref (s/gen ::org-ref)
                      issued-at (check-gen/one-of [(check-gen/return (t/now)) (s/gen ::issued-at)])
                      issued-by (s/gen ::issued-by)
                      received-at (s/gen ::received-at)
                      source (s/gen ::source)
                      query (s/gen ::query)]
                     (create-map query-type query-id org-ref issued-at issued-by received-at source query)))

(defmacro defquery
          "define a query with first arg the ns keyword of the query then the keyword of the spec"
          [queryk datak & options]
  (let [options (first options)
        ns *ns*]
    `(do
       (s/def ~queryk
         (s/with-gen (s/merge ::query-metadata (s/keys :req-un [~datak]))
                     #(gen/fmap (fn [[query-meta# data#]]
                                  (assoc query-meta# (keyword (name ~datak)) data#))
                                (gen/tuple (query-meta-gen ~queryk) (s/gen ~datak)))))

       (defmethod query-type ~queryk [_#] (s/get-spec ~queryk))

       (defn ~(-> (symbol (str "->" (name queryk)))
                  (vary-meta assoc :domain-asset {:name queryk
                                                  :type :query
                                                  :spec queryk})) ~(str "Create a query of type " queryk " with metadata (complementing the missing optional data, only :org-ref and :issued-by are mandatory keys in metadata) and data ") [metadata# data#]
         (let [merged-metadata# (query-metadata (assoc metadata# :query-type ~queryk))]
           (s/assert* ::query-metadata merged-metadata#)
           (assoc merged-metadata# :query data#)))

       (defn ~(symbol (str (name queryk) "-gen")) ~(str "Return a generator for the query type " queryk) []
         (check-gen/let [meta# (query-meta-gen ~queryk)
                         data# (s/gen ::query)]
                        (assoc meta# :query data#)))

       (let [query-desc# {:name     ~(name queryk)
                          :longname (clojure.reflect/typename (symbol ~queryk))
                          :ns       ~ns
                          :id       ~queryk
                          :kind     :query
                          :spec     ~queryk
                          :rely-on  ~(get options :rely-on)
                          :doc      ~(:doc options)}]
         (store! building-blocks query-desc#)
         (publish! bus query-desc#)
         query-desc#))))

(defn queries "return all the queries declared in scope" []
  (-> query-type methods keys set))
