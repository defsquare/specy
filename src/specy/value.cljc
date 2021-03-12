(ns specy.value
  (:require [clojure.string :as string]
            [specy.validation :as sv]
            [specy.protocols :refer [store!]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.registry :as sr]
            [malli.core :as mc]
            [malli.generator :as mg]))

#_(defmacro add-valuable-operations [name fields]
  (let [sb (gensym 'sb)
        value (gensym 'value)]
    `(extend-type ~name
       Valueable
       (to-string [~value]
                  (let [~sb (StringBuilder.)]
                    ~@(map (fn [field] (if (= field :space)
                                         `(.append ~sb " ")
                                         `(.append ~sb (get ~value ~(keyword (str field))))))
                           (interpose :space fields))
                    (.toString ~sb))))))

(defmacro defvalue
  "(defvalue value-name schema options & behaviors) where options is a map with
      - schema : schema that describes data structure. Can be a map or a ref to a schem
      - options : {:doc string - the docstring of this entity}

    Behaviors are express as named functions, which takes record instance as first parameter :
    (fn my-function [this] (do stuff here...))

    Usage :
    (defvalue MyValue
              [:map [:title string?]]
              {:doc \"Any documentation here\"}
              (fn say-it [this word] (str (:title this) \" with \" word)))

    (say-it (->myvalue {:title \"hello\"}) \"world\")"
  {:arglists '([name schema options & behaviors])}
  [value-name schema {:keys [doc] :as options} & behaviors]
  (let [interface (some->> (seq behaviors) (map #(take 2 (rest %))))
        implementations (some->> (seq behaviors) (map rest)) ;; TODO should handle function ref
        props (map first (rest schema))
        protocol-ref-symbol (symbol (str (string/capitalize value-name) "Procotol"))
        schema-ref-symbol (symbol (str value-name "-schema"))
        builder-ref-symbol (symbol (str "->" (string/lower-case value-name)))
        ns *ns*
        full-qualified-id (keyword (str ns) (clojure.string/lower-case (str value-name)))]
    `(do
       (def ~schema-ref-symbol ~schema)

       (defprotocol ~protocol-ref-symbol
         ~@interface)

       (defrecord ~value-name [~@(map symbol props)]
         ~protocol-ref-symbol
         ~@implementations)

       (defn ~builder-ref-symbol [m#]
         (sv/assert-schema ~schema-ref-symbol m#)
         (~(symbol (str "map->" value-name)) m#))

       (def
         ^{:doc    ~(str "Return true if x is a " name)
           :static true}
         ~(symbol (str value-name "?")) (fn ^:static ~(symbol (str value-name "?")) [x#] (sv/valid? ~full-qualified-id x#)))

       (let [value-desc# (array-map
                           :id ~full-qualified-id
                           :name ~(str value-name)
                           :ns  ~(str ns)
                           ;:longname (clojure.reflect/typename ~value-name)
                           :doc ~doc
                           :class ~value-name
                           :kind :value
                           :schema-ref (quote ~schema-ref-symbol)
                           :schema ~schema
                           :validation-fn (quote ~(symbol (str ns "/" value-name "?")))
                           :props (quote ~props)
                           :builder (quote ~(symbol (str "(" ns "/" builder-ref-symbol " " (->> props (map (fn [e] [e '...])) (into {})) ")"))))]
         (sr/register! ~full-qualified-id
                       #(instance? ~value-name %)
                       ~schema
                       (str "Should be a value of " ~value-name))
         (store! building-blocks value-desc#)
         value-desc#))))