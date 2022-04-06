(ns specy.query
  (:require [tick.core :as t]
            [malli.util :as mu]
            [specy.uuid :as uuid]
            [specy.validation :as sv]
            [specy.time :as time]
            [specy.protocols :refer [store!]]
            [specy.infra.repository :refer [building-blocks]])
  #?(:cljs (:require-macros [specy.query])))

(def metadata-schema [:map
                      [:id uuid?]
                      [:type keyword?]
                      [:issued-at [:and {:gen/elements [(t/instant)]} [:fn time/instant?]]] ;; FIXME by using :instance instead, (PR https://github.com/metosin/malli/pull/176)
                      [:issued-by any?]
                      [:correlation-id uuid?]])

(defn ->metadata [{:keys [id issued-at issued-by correlation-id from-event meta] :as metadata}]
  (cond-> {:id             (or id (uuid/random))
           :issued-at      (or issued-at (t/instant))
           :issued-by      (or issued-by (:published-by from-event))
           :correlation-id (or correlation-id (:correlation-id from-event) (uuid/random))}
          meta (assoc :meta meta)))

(defn- print-metadata-builder [m] (->> m (filter (partial not= :type)) (map (fn [e] [e nil])) (into {})))
(defn- print-payload-builder [m] (->> m (map (fn [e] [e nil])) (into {})))

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
                    [:payload ~schema]]
                   {:registry (specy.registry/get-registry)}))

       (defn
         ^{:doc    ~(str "Validate data from schema " schema-ref-symbol)
           :static true}
         ~(symbol (str query-name "?")) [query#] (sv/valid? ~schema-ref-symbol query#))

       (defn ~builder-ref-symbol
         ~(str "Create a query of type ")
         [metadata# data#]
         (sv/assert-schema ~schema-ref-symbol (assoc (->metadata metadata#)
                                             :type ~(keyword (str ns) (clojure.string/lower-case (str query-name)))
                                             :payload data#)))

       (let [query-desc# (array-map
                              :id ~(keyword (str ns) (clojure.string/lower-case (str query-name)))
                              :name ~(str query-name)
                              :ns  ~(str ns)
                              ;:longname (clojure.reflect/typename ~query-name)
                              :doc ~doc
                              :kind :query
                              :schema-ref (quote ~schema-ref-symbol)
                              :schema ~schema-ref-symbol
                              :validation-fn (quote ~(symbol (str "(" ns "/" query-name "? query)")))
                              :builder (quote ~(symbol (str "("
                                                            ns "/" builder-ref-symbol " "
                                                            (print-metadata-builder metadata-props)
                                                            (print-payload-builder props)
                                                            ")")))
                              :builder-from-event (quote ~(symbol (str "("
                                                                       ns "/" builder-ref-symbol " "
                                                                       "{:from-event event}"
                                                                       (print-payload-builder props)
                                                                       ")"))))]
         (store! building-blocks query-desc#)
         query-desc#))))


(comment
  (defquery my-query [:map [:title string?]] {:doc "Any documentation here"}))
