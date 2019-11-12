(ns specy.query
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query Spec                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::query-type keyword?)
(s/def ::query-id uuid?)
(s/def ::query any?)

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
          [queryk]
          `(do (s/def ~queryk
                 (s/with-gen ::query-metadata
                             #(query-meta-gen ~queryk)))
               (defmethod query-type ~queryk [_#] (s/get-spec ~queryk))

               (defn ~(symbol (str "->" (name queryk))) ~(str "Create a query of type " queryk " with metadata (complementing the missing optional data, only :org-ref and :issued-by are mandatory keys in metadata) and data ") [metadata# data#]
                 (let [merged-metadata# (query-metadata (assoc metadata# :query-type ~queryk))]
                   (s/assert* ::query-metadata merged-metadata#)
                   (assoc merged-metadata# :query data#)))

               (defn ~(symbol (str (name queryk) "-gen")) ~(str "Return a generator for the query type " queryk) []
                 (check-gen/let [meta# (query-meta-gen ~queryk)
                                 data# (s/gen ::query)]
                   (assoc meta# :query data#)))
               ~queryk))

(defn queries "return all the queries declared in scope" []
  (-> query-type methods keys set))
