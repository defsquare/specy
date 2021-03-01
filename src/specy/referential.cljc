(ns specy.referential
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.utils :refer [inspect operations parse-opts+specs]]

            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            [clojure.string :as string]
            [spec-tools.data-spec :as ds]))

(defmacro defreferential
  "(defreferential name [fields*]) "
  {:arglists '([name [& fields] & opts])}
  ([name fields]
   (let [inspected-fields (inspect building-blocks fields)
         fields-name (map :field inspected-fields)]
     `(defrecord ~name [~@fields-name]))))


(defmacro defreferential2
  "(defreferential name referential-spec) "
  {:arglists '([name referential-spec & opts])}
  ([name & args]
   (let [;;next lines deal with docstring optionality ...
         doc (when (string? (first args))
               (first args))
         ;;remove docstring from args
         args (if (string? (first args))
                (next args)
                args)
         [value-spec referential-data] args
         ns *ns*
         id (keyword (str ns) (clojure.string/lower-case (str name)))]
     `(do
        (s/assert* (ds/spec {:name ~id :spec ~value-spec}) ~referential-data)
        (def ~(symbol (str name "-spec")) (ds/spec {:name ~id
                                                    :spec ~value-spec}))
        (def ~name
          ~referential-data)

        (let [referential-desc# {:id       ~id
                                 :name     ~(str name)
                                 :ns       ~(str ns)
                                 :longname ~(clojure.reflect/typename name)
                                 :class    ~name
                                 :kind     :referential
                                 :spec     ~(symbol (str name "-spec"))
                                 :doc      ~doc}]
          (store! building-blocks referential-desc#)
          (publish! bus referential-desc#)
          referential-desc#)
        ))))


(comment

  (defreferential2 Tags
                   "A list of tag where tag is a string without spaces or special characters and limited in size to 42 chars"
                   [{:code (s/and string?
                                  #(empty? (filter (fn [c] (= \space c)) %))
                                  #(< (count %) 42))
                     :name string?}]
                   [{:code "computer" :name "Computer science course"}
                    {:code "en-language" :name "English course"}])

  Tags

  (gen/generate (s/gen Tags-spec))

  )