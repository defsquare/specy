(ns specy.value
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]
            [specy.domain :refer :all]
            [specy.utils :refer [inspect operations]]
            ))

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
                               `(.append ~sb (get ~value ~(keyword (str field)))))) (interpose :space fields))
           (.toString ~sb))))))



(defmacro defvalue
  "(defvalue name [fields*] protocol-name [operations*] options*) "
  {:arglists '([name [& fields] & opts+specs])}
  ([name fields & opts+specs]
   (let [inspected-fields (inspect fields)
         fields-name (map :field inspected-fields)
         [interfaces methods opts] (parse-opts+specs opts+specs)
         operations (operations methods)]
     `(do
        ~(if (not-empty opts+specs)
           `(defprotocol ~@opts+specs)
           `(defprotocol ~(symbol (str name "able"))))
        (defrecord ~name [~@fields-name] ~(if (not-empty opts+specs) (first opts+specs) (symbol (str name "able"))))
        (add-valuable-operations ~name ~fields-name)
        ;;return the value as a data structure
        {:name ~name
         :fields ~(vec (map #(dissoc % :field) inspected-fields))
         :interface ~(first interfaces)
         :operations ~operations
         }
        )
     )))
(s/def ::iso-code (every-pred string? #(= (count %) 3)))

(defvalue Currency [iso-code ::iso-code
                    numeric-code pos-int?
                    display-name string?
                    symbol string?
                    fraction-digits int?])

(defvalue Amount [qty pos-int?
                  currency Currency])


(defvalue Amount
  [qty int?
   currency Currency]
  Amountable
  (qty [this])
  (add [this other])
  (subtract [this other])
  (eq [this other])
  (currency [amount])
  (currency-iso-code [amount])
  (currency-numeric-code [amount])
  (currency-display-name [amount])
  (currency-symbol [amount])
  (currency-fraction-digits [amount]))

