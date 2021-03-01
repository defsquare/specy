(ns specy.event
  (:require [tick.alpha.api :as t]
            [malli.util :as mu]
            [specy.uuid :as uuid]
            [specy.validation :as sv]
            [specy.time :as time]
            [specy.protocols :refer [store!]]
            [specy.infra.repository :refer [building-blocks]])
  #?(:cljs (:require-macros [specy.event])))

(def metadata-schema [:map
                      [:id uuid?]
                      [:type keyword?]
                      [:published-at [:and {:gen/elements [(t/instant)]} [:fn time/instant?]]] ;; FIXME by using :instance instead, (PR https://github.com/metosin/malli/pull/176)
                      [:published-by any?]
                      [:correlation-id uuid?]])

(defn ->metadata [{:keys [id published-at published-by correlation-id from-event from-command meta] :as metadata}]
  (cond-> {:id             (or id (uuid/random))
           :published-at   (or published-at (t/instant))
           :published-by   (or published-by (:issued-by from-event) (:issued-by from-command))
           :correlation-id (or correlation-id (:correlation-id from-event) (:correlation-id from-command) (uuid/random))}
          meta (assoc :meta meta)))

(defn- print-metadata-builder [m] (->> m (filter (partial not= :type)) (map (fn [e] [e nil])) (into {})))
(defn- print-payload-builder [m] (->> m (map (fn [e] [e nil])) (into {})))

(defmacro defevent
  "(defevent event-name schema options) where options is a map with :
      - schema : schema that describes data structure. Can be a map or a ref to a schema
      - options : {:doc string - the docstring of this entity}

    Usage :
    (defevent my-event [:map [:title string?]] {:doc \"Any documentation here\"})"
  [event-name schema {:keys [doc] :as options}]
  (let [ns *ns*
        metadata-props (map first (rest metadata-schema))
        props (map first (rest schema))
        schema-ref-symbol (symbol (str event-name "-schema"))
        builder-ref-symbol (symbol (str "->" (name event-name)))]
    `(do

       (def ~schema-ref-symbol
         (mu/merge metadata-schema
                   [:map
                    [:payload ~schema]]))

       (defn
         ^{:doc    ~(str "Validate data from schema " schema-ref-symbol)
           :static true}
         ~(symbol (str event-name "?")) [event#] (sv/valid? ~schema-ref-symbol event#))

       (defn ~builder-ref-symbol
         ~(str "Create a event of type ")
         [metadata# data#]
         (sv/assert-schema ~schema-ref-symbol (assoc (->metadata metadata#)
                                             :type ~(keyword (str ns) (clojure.string/lower-case (str event-name)))
                                             :payload data#)))

       (let [event-desc# (array-map
                           :id ~(keyword (str ns) (clojure.string/lower-case (str event-name)))
                           :name ~(str event-name)
                           :ns  ~(str ns)
                           ;:longname (clojure.reflect/typename ~event-name)
                           :doc ~doc
                           :kind :event
                           :schema-ref (quote ~schema-ref-symbol)
                           :schema ~schema-ref-symbol
                           :validation-fn (quote ~(symbol (str "(" ns "/" event-name "? event)")))
                           :builder (quote ~(symbol (str "("
                                                         ns "/" builder-ref-symbol " "
                                                         (print-metadata-builder metadata-props)
                                                         (print-payload-builder props)
                                                         ")")))
                           :builder-from-command (quote ~(symbol (str "("
                                                                      ns "/" builder-ref-symbol " "
                                                                      "{:from-command command}"
                                                                      (print-payload-builder props)
                                                                      ")")))
                           :builder-from-query (quote ~(symbol (str "("
                                                                    ns "/" builder-ref-symbol " "
                                                                    "{:from-query query}"
                                                                    (print-payload-builder props)
                                                                    ")"))))]
         #?(:clj (store! building-blocks event-desc#))
         event-desc#))))


(comment
  (defevent my-event [:map [:title string?]] {:doc "Any documentation here"}))