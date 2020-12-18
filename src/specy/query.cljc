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
            [specy.utils :refer [inspect operations parse-opts+specs]]
            [malli
             [core :as mc]
             [generator :as mg]
             [util :as mu]
             [error :as me]]))

(def metadata-schema [:map
                      [:id uuid?]
                      [:type keyword?]
                      [:issued-at [:and {:gen/elements [(t/instant)]} [:fn time/instant?]]] ;; FIXME by using :instance instead, (PR https://github.com/metosin/malli/pull/176)
                      [:correlation-id string?]])

(defn ->metadata [{:keys [id type issued-at correlation-id] :as metadata}]
  (merge {:id             (or id (uuid/random))
          :type           type
          :issued-at      (or issued-at (t/instant))
          :correlation-id (or correlation-id (uuid/random))}
         metadata))

(defn assert-schema [schema data]
  (if (mc/validate schema data)
    data
    (let [explain (mc/explain schema data)]
      (throw (ex-info (str "Not conform to schema :\n" (me/humanize explain) "")
                      explain)))))

(defmacro defquery
  "(defquery query-name schema options) where options is a map with :
      - schema : schema that describes data structure. Can be a map or a ref to a schema
      - options : {:doc string - the docstring of this entity}

    Usage :
    (defquery my-query [:map [:title string?]] {:doc \"Any documentation here\"})"
  [query-name schema {:keys [doc] :as options}]
  (let [ns *ns*
        metadata-props (map first (rest metadata-schema))
        props (map first (rest schema))
        schema-ref-symbol (symbol (str query-name "-schema"))
        builder-ref-symbol (symbol (str "->" (name query-name)))]
    `(do

       (def ~schema-ref-symbol
         (mu/merge metadata-schema
                   [:map
                    [:payload ~schema]]))

       (defn ~builder-ref-symbol
         ~(str "Create a query of type ")
         [metadata# data#]
         (assert-schema ~schema-ref-symbol (assoc (->metadata metadata#) :payload data#)))

       (let [query-desc# (array-map
                           :id ~(keyword (str ns) (clojure.string/lower-case (str query-name)))
                           :name ~(str query-name)
                           ;:longname (clojure.reflect/typename ~query-name)
                           :doc ~doc
                           :ns ~ns                        ;;caller ns
                           :kind :query
                           :schema-ref (quote ~schema-ref-symbol)
                           :schema ~schema-ref-symbol
                           :builder (quote ~(symbol (str "("
                                                         ns "/" builder-ref-symbol " "
                                                         (->> metadata-props (map (fn [e] [e '...])) (into {}))
                                                         (->> props (map (fn [e] [e '...])) (into {}))
                                                         ")"))))]
         (store! building-blocks query-desc#)
         (publish! bus query-desc#)
         query-desc#))))


(comment
  (defquery my-query [:map [:title string?]] {:doc "Any documentation here"}))
