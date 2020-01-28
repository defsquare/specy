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
            ))

(defmacro defreferential
  "(defreferential name [fields*]) "
  {:arglists '([name [& fields]  & opts])}
  ([name fields]
   (let [inspected-fields (inspect building-blocks fields)
         fields-name (map :field inspected-fields)]
     `(defrecord ~name [~@fields-name])
     )))


