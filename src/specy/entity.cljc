(ns specy.entity
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.utils :refer [inspect operations parse-opts+specs]]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]

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
    (fn my-function [] (do stuff here...))

    Usage :
    (defentity MyEntity
              {:doc \"Any documentation here\" :schema [:map [:title string?]]}
              (fn say-it [this word] (str (:title this) \" with \" word)))

    (say-it (->myentity {:title \"hello\"}) \"world\")"
  {:arglists '([name options & behaviors])}
  [entity-name schema {:keys [doc] :as options} & behaviors]
  (let [interface (map #(take 2 (rest %)) behaviors)
        implementations (map rest behaviors)                ;; TODO should handle function ref
        props (map first (rest schema))
        schema-ref-symbol (symbol (str entity-name "-schema"))
        builder-ref-symbol (symbol (str "->" (string/lower-case entity-name)))
        ns *ns*]
    `(do
       (def ~schema-ref-symbol ~schema)

       (defprotocol ~(symbol (str (string/capitalize entity-name) "Procotol"))
         ~@interface)

       (defrecord ~entity-name [~@(map symbol props)]
         ~(symbol (str (string/capitalize entity-name) "Procotol"))
         ~@implementations)

       (defn ~builder-ref-symbol [m#]
         (assert-schema ~schema-ref-symbol m#)
         (~(symbol (str "map->" entity-name)) m#))

       (def
         ^{:doc    ~(str "Return true if x is a " name)
           :static true}
         ~(symbol (str entity-name "?")) (fn ^:static ~(symbol (str entity-name "?")) [x#] (instance? ~entity-name x#)))

       (let [entity-desc# (array-map
                            :id ~(keyword (str ns) (clojure.string/lower-case (str entity-name)))
                            :name ~(str entity-name)
                            :longname (clojure.reflect/typename ~entity-name)
                            :doc ~doc
                            :ns ~ns                       ;;caller ns
                            :class ~entity-name
                            :kind :entity
                            :schema-ref (quote ~schema-ref-symbol)
                            :schema ~schema
                            :validation-fn (quote ~(symbol (str ns "/" entity-name "?")))
                            :props (quote ~props)
                            :builder (quote ~(symbol (str "(" ns "/" builder-ref-symbol " " (->> props (map (fn [e] [e '...])) (into {})) ")"))))]
         (store! building-blocks entity-desc#)
         (publish! bus entity-desc#)
         entity-desc#))))

(comment

  (macroexpand-1 '(defentity MyEntity
                             {:title {:required? true} string?
                                     :name string?
                              }
                             [:map
                              [:title string?]
                              [:price Amount]]
                             {:doc "Any documentation here"}
                             ;(fn say-it [this word] (str (:title this) " with " word))
                             ))

  (say-it (->myentity {:title "hello"}) "world")

  (say (specy.entity/->notice {:id 1, :ean "12134"}) "coucou")



  )