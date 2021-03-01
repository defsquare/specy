(ns specy.rule
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.infra.bus :refer [bus]]
            [specy.infra.repository :refer [building-blocks]]
            ))

(defmacro defrule
  "(defrule name docstring? building-block pred) "
  {:arglists '([name docstring? building-block? pred])}
  ([name & args]
   (let [;;next lines deal with docstring and building-block optionality ok that's ugly should be extracted in a fn...
         m (if (string? (first args))
             {:doc (first args)}
             {})
         ;;remove docstring from args
         args (if (string? (first args))
                (rest args)
                args)
         ;;building-block is optional
         m (if (not (fn? (first args)))
             (assoc m :building-block (first args))
             m)
         ;;remove building-block from args
         args (if (contains? m :building-block)
                (rest args)
                args)
         pred (first args)

         ns *ns*]
     `(do
        ;;return the value as a data structure
        (def ~name ~pred)
        (let [rule-desc# {:name           ~(str name)
                          :ns             ~(str ns)
                          :id             ~(keyword (str ns) (clojure.string/lower-case (str name)))
                          :fn             ~pred
                          :building-block ~(:building-block m)
                          :kind           :rule
                          :doc            ~(:doc m)
                          }]
          (store! building-blocks rule-desc#)
          (publish! bus rule-desc#)
          rule-desc#)))))
