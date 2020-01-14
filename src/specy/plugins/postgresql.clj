(ns specy.plugins.postgresql
  (:require
   [specy.protocols :refer [EventBus publish! subscribe]]
   [specy.core-async-bus :refer [bus]]))

(subscribe bus :value (fn [v]
                        (println "receive value from postgresql" v)))

(subscribe bus :entity (fn [e]
                         (println "receive entity from postgresql" e)))
