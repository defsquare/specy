(ns specy.infra.bus
  (:require
   [clojure.tools.logging :as log :refer [info]]
   #?(:clj  [clojure.core.async :refer [chan <! >! timeout pub sub unsub unsub-all go go-loop buffer dropping-buffer]])
   #?(:cljs [cljs.core.async :refer [chan <! >! timeout pub sub unsub unsub-all go go-loop buffer dropping-buffer]])
   [specy.protocols :refer [EventBus publish! subscribe]]
   ))

(defonce BUFFER_SIZE 16)

(defrecord CoreAsyncEventBus [name pub-chan publication]
  EventBus
  (publish! [_ event]
    (info "publish event " event)
    (go (>! pub-chan event)))
  (subscribe [_ f]
    (throw (UnsupportedOperationException. "Can't subscribe without a key to CoreAsyncEventBus (though possible with mult but not implemented, left as an exercise to the reader)")))
  (subscribe [_ k f]
    (info "subscribe to event with key " k)
    (let [sub-chan (chan (buffer BUFFER_SIZE))]
      (sub publication k sub-chan)
      (go-loop []
        (f (<! sub-chan))
        (recur))))
  (stop [_]
    (unsub-all publication)))

(defn core-async-event-bus
  ([extraction-key-fn]
   (core-async-event-bus "core async event bus" extraction-key-fn))
  ([name extraction-key-fn]
   (core-async-event-bus name extraction-key-fn nil))
  ([name extraction-key-fn buffer]
   (let [pub-chan (if buffer (chan buffer) (chan))]
     (->CoreAsyncEventBus name pub-chan (pub pub-chan extraction-key-fn)))))

(defn core-async-event-bus-dropping-buffer "a core async bus with a dropping buffer (can be used in test to avoir blocked publish)"
  [name extraction-key-fn]
  (core-async-event-bus name extraction-key-fn (dropping-buffer BUFFER_SIZE)))

(def bus (core-async-event-bus "specy-bus" :kind (buffer BUFFER_SIZE)))

(subscribe bus :value (fn [v] (println "receive value " v)))
(subscribe bus :entity (fn [e] (println "receive entity " e)))
