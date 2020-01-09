(ns specy.referential
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.core-async-bus :refer [bus]]
            [specy.utils :refer [inspect operations parse-opts+specs]]

            ))

(defmacro defreferential
  "(defreferential name [fields*]) "
  {:arglists '([name [& fields]  & opts])}
  ([name fields]
   (let [inspected-fields (inspect fields)
         fields-name (map :field inspected-fields)]
     (println fields-name)
     `(defrecord ~name [~@fields-name])
     (println inspected-fields)
     
     )))


