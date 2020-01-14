(ns specy.infra.repository-test
  (:require
   [clojure.test :as t]
   [specy.protocols :refer [find-value-by-class subscribe store!]]
   [specy.infra.repository :as repo :refer [building-blocks]]
   [specy.infra.bus :refer [bus]]
   )
  (:import [specy.example.amount Amount]))


(t/deftest building-blocks-repo-test
  (t/testing "defining a value with defvalue from example ns should store the value metadata in repository"
    (t/is (= "Amount" (:name (find-value-by-class building-blocks Amount))))))

(subscribe bus :value (fn [x]
                        (println "Store value " (:id x))
                        (store! building-blocks x)))

(subscribe bus :entity (fn [x]
                         (println "Store entity " (:id x))
                         (store! building-blocks x)))
