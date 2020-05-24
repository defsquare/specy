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
            [spec-tools.data-spec :as ds]))

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] (list macroname arg)) args)))

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


(comment
  (macroexpand '(defvalue2 MyValue {:id string? :foo string?}
                           (from-string [this] (get this :id))))


  (defvalue2 MyValue {:id string? :foo string?}
             (from-string [this] (get this :id)))
  (to-string (->myvalue {:id "lol" :foo "bar"}))
  )

(defmacro defvalue
  "(defvalue name docstring? [fields*] protocol-name [operations*] options*) "
  {:arglists '([name docstring? [& fields] & opts+specs])}
  ([name & args]
   (let [
         ;;next lines deal with docstring optionality ...
         m (if (string? (first args))
             {:doc (first args)}
             {})
         ;;remove docstring from args
         args (if (string? (first args))
                (next args)
                args)
         fields (when (vector? (first args)) (first args))
         opts+specs (rest args)

         inspected-fields (inspect building-blocks fields)
         fields-name (map :field inspected-fields)
         [interfaces methods opts] (parse-opts+specs opts+specs)
         operations (operations methods)
         ns *ns*]
     `(do
        ~(if (not-empty opts+specs)
           `(defprotocol ~@opts+specs)
           `(defprotocol ~(symbol (str name "able"))))
        (defrecord ~name [~@fields-name])                   ;~(if (not-empty opts+specs) (first opts+specs) (symbol (str name "able")))
        (add-valuable-operations ~name ~fields-name)
        ;;return the value as a data structure
        (let [value-desc# {:name       ~(str name)
                           :longname   (clojure.reflect/typename ~name)
                           :ns         ~ns                  ;;caller ns
                           :id         ~(keyword (str ns) (clojure.string/lower-case (str name)))
                           :class      ~name
                           :kind       :value
                           :fields     ~(vec (map #(dissoc % :field) inspected-fields))
                           :interface  ~(first interfaces)
                           :operations ~operations
                           :doc        ~(:doc m)
                           }]
          (store! building-blocks value-desc#)
          (publish! bus value-desc#)
          value-desc#)))))

(defmacro defvalue2
  "(defvalue name docstring? spec operations*) "
  {:arglists '([name value-spec & operations])}
  ([name & args]
   (let [
         ;;next lines deal with docstring optionality ...
         doc (when (string? (first args))
             (first args))
         ;;remove docstring from args
         args (if (string? (first args))
                (next args)
                args)
         value-spec (first args)

         fields (map symbol (keys value-spec))
         operations (rest args)
         methods (map #(take 2 %) operations)
         ns *ns*
         id (keyword (str ns) (clojure.string/lower-case (str name)))]
     `(do
        (def ~(symbol (str name "-spec")) (ds/spec {:name ~id
                                                    :spec ~value-spec}))

        (defprotocol ~(symbol (str (string/capitalize name) "Procotol"))
          ~@methods)

        (defrecord ~name [~@fields]
          ~(symbol (str (string/capitalize name) "Procotol"))
          ~@methods)
        (add-valuable-operations ~name ~(keys value-spec))

        (def
          ^{:doc    ~(str "Return true if x is a " name)
            :static true}
          ~(symbol (str name "?")) (fn ^:static ~(symbol (str name "?")) [x#] (instance? ~name x#)))

        (defn ~(symbol (str "->" (string/lower-case name))) [m#]
          (s/assert* ~(symbol (str name "-spec")) m#)
          (~(symbol (str "map->" name)) m#))

        ;;return the value as a data structure
        (let [value-desc# {:id         ~id
                           :name       ~(str name)
                           :longname   (clojure.reflect/typename ~name)
                           :ns         ~ns                  ;;caller ns
                           :class      ~name
                           :kind       :value
                           :spec       ~(symbol (str name "-spec"))
                           ;:interface  ~(first interfaces)
                           ;:operations ~operations
                           :doc        ~doc}]
          (store! building-blocks value-desc#)
          (publish! bus value-desc#)
          value-desc#)))))