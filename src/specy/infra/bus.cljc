(ns specy.infra.bus
  (:require
   [specy.internal.logging :refer [debug]]
   #?(:clj  [clojure.core.async :refer [chan <! >! timeout pub sub unsub unsub-all go go-loop buffer dropping-buffer]])
   #?(:cljs [cljs.core.async :refer [chan <! >! timeout pub sub unsub unsub-all go go-loop buffer dropping-buffer]])
   [specy.protocols :refer [EventBus publish! subscribe]]
   ))

(defonce BUFFER_SIZE 16)

(defrecord CoreAsyncEventBus [name pub-chan publication]
  EventBus
  (publish! [_ event]
    (debug "publish event " event)
    (go (>! pub-chan event)))
  (subscribe [_ f]
    (throw (#?(:clj UnsupportedOperationException.
               :cljs js/Error) "Can't subscribe without a key to CoreAsyncEventBus (though possible with mult but not implemented, left as an exercise to the reader)")))
  (subscribe [_ k f]
    (debug "subscribe to event with key " k)
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


(def assets (atom {}))

(subscribe bus :value (fn [v] (swap! assets assoc (:id v) v) (println "receive value " v)))
(subscribe bus :entity (fn [e] (swap! assets assoc (:id e) e) (println "receive entity " e)))
(subscribe bus :command (fn [c] (swap! assets assoc (:id c) c) (println "receive command " c)))
(subscribe bus :query (fn [q] (swap! assets assoc (:id q) q) (println "receive query " q)))
(subscribe bus :event (fn [e] (swap! assets assoc (:id e) e) (println "receive event " e)))
(subscribe bus :referential (fn [e] (swap! assets assoc (:id e) e) (println "receive referential " e)))