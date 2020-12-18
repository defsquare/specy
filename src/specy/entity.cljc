(ns specy.entity
  (:require [clojure.string :as string]
            [malli.core :as mc]
            [specy.validation :as sv]
            [specy.protocols :refer [store!]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.registry :as sr]

            [spec-tools.data-spec :as ds]
            [spec-tools.core :as cs]
            [clojure.string :as string]
            [malli.core :as mc]
            [malli.generator :as mg]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]))

(defn assert-schema [schema data]
  (if (mc/validate schema data)
    data
    (let [explain (mc/explain schema data)]
      (throw (ex-info (str "Not conform to schema :\n" (me/humanize explain) "")
                      explain)))))

(defmacro defentity
  "(defentity entity-name schema options & behaviors) where options is a map with
       - schema : schema that describes data structure. Can be a map or a ref to a schema
       - options : {:doc string - the docstring of this entity}

    Behaviors are express as named functions, which takes record instance as first parameter :
    (fn my-function [this] (do stuff here...))

    Usage :
    (defentity MyEntity
              [:map [:title string?]]
              {:doc \"Any documentation here\" :schema [:map [:title string?]]}
              (fn say-it [this word] (str (:title this) \" with \" word)))

    (say-it (->myentity {:title \"hello\"}) \"world\")"
  {:arglists '([name schema options & behaviors])}
  [entity-name schema {:keys [doc] :as options} & behaviors]
  (let [interface (map #(take 2 (rest %)) behaviors)
        implementations (map rest behaviors)                ;; TODO should handle function ref
        props (map first (rest schema))
        schema-ref-symbol (symbol (str entity-name "-schema"))
        builder-ref-symbol (symbol (str "->" (string/lower-case entity-name)))
        ns *ns*
        full-qualified-id (keyword (str ns) (clojure.string/lower-case (str entity-name)))]
    `(do
       (def ~schema-ref-symbol ~schema)

       (defprotocol ~(symbol (str (string/capitalize entity-name) "Procotol"))
         ~@interface)

       (defrecord ~entity-name [~@(map symbol props)]
         ~(symbol (str (string/capitalize entity-name) "Procotol"))
         ~@implementations)

       (defn ~builder-ref-symbol [m#]
         (sv/assert-schema ~schema-ref-symbol m#)
         (~(symbol (str "map->" entity-name)) m#))

       (def
         ^{:doc    ~(str "Return true if x is a " name)
           :static true}
         ~(symbol (str entity-name "?")) (fn ^:static ~(symbol (str entity-name "?")) [x#] (sv/valid? ~full-qualified-id x#)))

       (let [entity-desc# (array-map
                            :id ~(keyword (str ns) (clojure.string/lower-case (str entity-name)))
                            :name ~(str entity-name)
                            :ns ~(str ns)
                            :longname (clojure.reflect/typename ~entity-name)
                            :doc ~doc
                            :class ~entity-name
                            :kind :entity
                            :schema-ref (quote ~schema-ref-symbol)
                            :schema ~schema
                            :validation-fn (quote ~(symbol (str ns "/" entity-name "?")))
                            :props (quote ~props)
                            :builder (quote ~(symbol (str "(" ns "/" builder-ref-symbol " " (->> props (map (fn [e] [e '...])) (into {})) ")"))))]
         (sr/register! ~full-qualified-id
                       #(instance? ~entity-name %)
                       ~schema
                       (str "Should be an instance of " ~entity-name))
         (store! building-blocks entity-desc#)
         entity-desc#))))

(comment

  (macroexpand-1 '(defentity MyEntity
                             [:title {:required? true} string?
                              :name string?
                              ]
                             [:map
                              [:title string?]
                              [:price Amount]]
                             {:doc "Any documentation here"}
                             ;(fn say-it [this word] (str (:title this) " with " word))
                             ))

  (say-it (->myentity {:title "hello"}) "world")

  (say (specy.entity/->notice {:id 1, :ean "12134"}) "coucou")



  )
