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

            ))

(defmacro defentity
  "(defentity name [fields*] protocol-name [operations*] options*) "
  {:arglists '([name [& fields] & opts+specs])}
  ([name fields & opts+specs]
   (let [inspected-fields (inspect building-blocks fields)
         fields-name (map :field inspected-fields)
         [interfaces methods opts] (parse-opts+specs opts+specs)
         operations (operations methods)
         ns *ns*]
     `(do
        ~(if (not-empty opts+specs)
           `(do
              (defprotocol ~@opts+specs)
              (defrecord ~name [~@fields-name] ~(first opts+specs)))
           `(defrecord ~name [~@fields-name]))
        ;;TODO create the repository interface associated to that entity ? or build a defrepository macro ?
        (let [entity-desc# (merge {:id         ~(keyword (str ns) (clojure.string/lower-case (str name)))
                                   :name       (clojure.reflect/typename ~name)
                                   :longname   (str ~ns "/" (clojure.reflect/typename ~name))
                                   :ns         ~ns ;;caller ns
                                   :class      ~name
                                   :kind       :entity
                                   :fields     ~(vec (map #(dissoc % :field) inspected-fields))
                                   :operations ~operations}
                                  ~(when (not-empty opts+specs)
                                     :interface ~(first interfaces)))]
          (store! building-blocks entity-desc#)
          (publish! bus entity-desc#)
          entity-desc#)))))
