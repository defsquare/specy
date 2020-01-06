(ns specy.value
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]
            [specy.domain :refer :all]
            ))

(defn pred-spec-or-class? [x]
  (cond
    (and (keyword? x) (s/get-spec x)) :spec
    (var? (resolve x)) (when-let [fn? (fn? (deref (resolve x)))]
                                                      (when fn? (let [argslist (:arglists (meta #'int?))]
                                                                  :pred)))
    (class? (type (resolve x))) :class
    :default (throw (ex-info "pred spec or class not found" {:x x}))))

(defn inspect [fields]
  (let [m (apply hash-map fields)]
    (map (fn [[field-name x]]
           {:field field-name
            :field-name (str field-name)
            :ref x
            :kind (pred-spec-or-class? x)}) m)))

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
  {:arglists '([name [& fields] & opts+sigs])}
  ([name fields & opts+sigs]
   (println name)
   (println fields)
   (println opts+sigs)
   (let [inspected-fields (inspect fields)
         fields-name (map :field inspected-fields)]
     (println fields-name)
     (println inspected-fields)
     `(do
        ~(if (not-empty opts+sigs)
           `(defprotocol ~@opts+sigs)
           `(defprotocol ~(symbol (str name "able"))))
        (defrecord ~name [~@fields-name] ~(symbol (str name "able")))
        (add-valuable-operations ~name ~fields-name))
     )))
(s/def ::iso-code (every-pred string? #(= (count %) 3)))

(defvalue Currency [iso-code ::iso-code
                    numeric-code pos-int?
                    display-name string?
                    symbol string?
                    fraction-digits int?])

(defvalue Amount [qty pos-int?
                  currency Currency])



