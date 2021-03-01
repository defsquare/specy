(ns specy.value
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            [specy.utils :refer [inspect operations parse-opts+specs]]
            [clojure.string :as string]
            [spec-tools.data-spec :as ds]
            [malli
             [core :as mc]
             [generator :as mg]
             [util :as mu]
             [error :as me]]))

(defmacro add-valuable-operations [name fields]
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

(defn assert-schema [schema data]
  (if (mc/validate schema data)
    data
    (let [explain (mc/explain schema data)]
      (throw (ex-info (str "Not conform to schema :\n" (me/humanize explain) "")
                      explain)))))

(defmacro defvalue
  "(defvalue value-name schema options & behaviors) where options is a map with
      - schema : schema that describes data structure. Can be a map or a ref to a schem
      - options : {:doc string - the docstring of this entity}

    Behaviors are express as named functions, which takes record instance as first parameter :
    (fn my-function [] (do stuff here...))

    Usage :
    (defvalue MyValue
              {:doc \"Any documentation here\" :schema [:map [:title string?]]}
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
        ns *ns*]
    `(do
       (def ~schema-ref-symbol ~schema)

       (defprotocol ~protocol-ref-symbol
         ~@interface)

       (defrecord ~value-name [~@(map symbol props)]
         ~protocol-ref-symbol
         ~@implementations)

       (defn ~builder-ref-symbol [m#]
         (assert-schema ~schema-ref-symbol m#)
         (~(symbol (str "map->" value-name)) m#))

       (def
         ^{:doc    ~(str "Return true if x is a " name)
           :static true}
         ~(symbol (str value-name "?")) (fn ^:static ~(symbol (str value-name "?")) [x#] (instance? ~value-name x#)))

       (let [value-desc# (array-map
                           :id ~(keyword (str ns) (clojure.string/lower-case (str value-name)))
                           :name ~(str value-name)
                           :ns  ~(str ns)
                           :longname (clojure.reflect/typename ~value-name)
                           :doc ~doc
                           :class ~value-name
                           :kind :value
                           :schema-ref (quote ~schema-ref-symbol)
                           :schema ~schema
                           :validation-fn (quote ~(symbol (str ns "/" value-name "?")))
                           :props (quote ~props)
                           :builder (quote ~(symbol (str "(" ns "/" builder-ref-symbol " " (->> props (map (fn [e] [e '...])) (into {})) ")"))))]
         (store! building-blocks value-desc#)
         (publish! bus value-desc#)
         value-desc#))))

(comment

  (defvalue MyValue
            [:map [:title string?]]
            {:doc "Any documentation here"}
            ;(fn say-it [this word] (str (:title this) " with " word))
            )

  (defrecord Toto [title])
  (specy.value/MyValue? (->myvalue {:title "hello"}))

  (say (specy.value/->notice {:id 1, :ean "12134"}) "coucou")

  )