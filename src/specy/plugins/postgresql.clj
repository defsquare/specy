(ns specy.plugins.postgresql
  (:require
   [specy.protocols :refer [EventBus publish! subscribe]]
   [specy.infra.bus :refer [bus]]))

(def type-mappings {string? "text"})

(subscribe bus :value (fn [v]
                        (println "receive value from postgresql" v)))

(subscribe bus :entity (fn [e]
                         (println "receive entity from postgresql" e)))

(subscribe bus :repository (fn [repo]
                            (println "receive repository " repo)))
