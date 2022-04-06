(ns specy.infra.repository
  (:require
    [clojure.set :refer [index]]
    [specy.protocols :refer [BuildingBlocksRepository]]))

(defrecord BuildingBlocksRepositoryInMemory
  [db indexes]
  BuildingBlocksRepository
  (store! [repo x]
    (swap! db assoc (:id x) x)
    ;;re-index
    (swap! indexes assoc :by-class (index (vals @db) [:class]))
    (swap! indexes assoc :by-longname (index (vals @db) [:longname]))
    (swap! indexes assoc :by-kind (index (vals @db) [:kind]))
    x)
  (all [repo] (deref db))
  #?(:clj
     (find-value-by-class [repo c]
       (let [resolved-class (resolve c)
             idx (:by-class @indexes)
             v (get idx {:class resolved-class})]
         v))))

(def building-blocks (->BuildingBlocksRepositoryInMemory (atom {}) (atom {})))
(comment
  @(:db building-blocks))

