(ns specy.entity
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.test.check.generators :as check-gen]

            [specy.protocols :refer :all]
            [specy.core-async-bus :refer [bus]]
            [specy.utils :refer [inspect operations parse-opts+specs]]
            ))

(defmacro defentity
  "(defentity name [fields*] protocol-name [operations*] options*) "
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
        ;;TODO create the repository interface associated to that entity ? or build a defrepository macro ?
        (let [entity-desc# {:name ~name
                           :kind :entity
                           :fields ~(vec (map #(dissoc % :field) inspected-fields))
                           :interface ~(first interfaces)
                           :operations ~operations}]
          (publish! bus entity-desc#)
          entity-desc#)))))
                                        ;
