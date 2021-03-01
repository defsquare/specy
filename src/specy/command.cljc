(ns specy.command
  (:require [tick.alpha.api :as t]
            [malli.util :as mu]
            [specy.uuid :as uuid]
            [specy.validation :as sv]
            [specy.time :as time]
            [specy.protocols :refer [store!]]
            [specy.infra.repository :refer [building-blocks]])
  #?(:cljs (:require-macros [specy.command])))

(def metadata-schema [:map
                      [:id uuid?]
                      [:type keyword?]
                      [:issued-by any?]
                      [:issued-at [:and {:gen/elements [(t/instant)]} [:fn time/instant?]]] ;; FIXME by using :instance instead, (PR https://github.com/metosin/malli/pull/176)
                      [:correlation-id uuid?]])

(defn ->metadata [{:keys [id issued-at issued-by correlation-id from-event meta] :as metadata}]
  (cond-> {:id             (or id (uuid/random))
           :issued-at      (or issued-at (t/instant))
           :issued-by      (or issued-by (:issued-by from-event))
           :correlation-id (or correlation-id (:correlation-id from-event) (uuid/random))}
          meta (assoc :meta meta)))

(defn- print-metadata-builder [m] (->> m (filter (partial not= :type)) (map (fn [e] [e nil])) (into {})))
(defn- print-payload-builder [m] (->> m (map (fn [e] [e nil])) (into {})))

(defmacro defcommand
  "(defcommand command-name schema options) where options is a map with :
      - schema : schema that describes data structure. Can be a map or a ref to a schema
      - options : {:doc string - the docstring of this entity}

    Usage :
    (defcommand my-command [:map [:title string?]] {:doc \"Any documentation here\"})"
  [command-name schema {:keys [doc] :as options}]
  (let [ns *ns*
        metadata-props (map first (rest metadata-schema))
        props (map first (rest schema))
        schema-ref-symbol (symbol (str command-name "-schema"))
        builder-ref-symbol (symbol (str "->" (name command-name)))]
    `(do

       (def ~schema-ref-symbol
         (mu/merge metadata-schema
                   [:map
                    [:payload ~schema]]))

       (defn
         ^{:doc    ~(str "Validate data from schema " schema-ref-symbol)
           :static true}
         ~(symbol (str command-name "?")) [command#] (sv/valid? ~schema-ref-symbol command#))

       (defn ~builder-ref-symbol
         ~(str "Create a command of type ")
         [metadata# data#]
         (sv/assert-schema ~schema-ref-symbol (assoc (->metadata metadata#)
                                                :payload data#
                                                :type ~(keyword (str ns) (clojure.string/lower-case (str command-name))))))

       (let [command-desc# (array-map
                             :id ~(keyword (str ns) (clojure.string/lower-case (str command-name)))
                             :name ~(str command-name)
                             :ns ~(str ns)
                             ;:longname (clojure.reflect/typename ~command-name)
                             :doc ~doc
                             :kind :command
                             :schema-ref (quote ~schema-ref-symbol)
                             :schema ~schema-ref-symbol
                             :validation-fn (quote ~(symbol (str "(" ns "/" command-name "? command)")))
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
         (store! building-blocks command-desc#)
         command-desc#))))

(comment

  (defcommand my-command
              [:map [:title string?]]
              {:doc "Any documentation here"})
  (mg/generate add-title-schema)
  (:builder (defcommand add-title
                        [:map [:title string?]]
                        {:doc "Any documentation here"}))
  )