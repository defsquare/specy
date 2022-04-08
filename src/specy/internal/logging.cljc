(ns specy.internal.logging
  (:refer-clojure :exclude [time])
  (:require #?(:clj  [clojure.tools.logging :as log]
               :cljs [goog.log :as glog])
            #?(:cljs [goog.log :as glog]))
  #?(:cljs (:import goog.debug.Console)))

#?(:cljs
   (def logger
     (glog/getLogger "app")))

#?(:cljs
   (def levels {:severe goog.log.Level.SEVERE
                :warning goog.log.Level.WARNING
                :info goog.log.Level.INFO
                :config goog.log.Level.CONFIG
                :fine goog.log.Level.FINE
                :finer goog.log.Level.FINER
                :finest goog.log.Level.FINEST}))

#?(:cljs
   (defn log-to-console! []
         (.setCapturing (goog.debug.Console.) true)))

#?(:cljs
   (defn set-level! [level]
         (.setLevel logger (get levels level (:info levels)))))

(defn fmt [msgs]
  (apply str (interpose " " (map pr-str msgs))))

(defn info [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/info msg)
       :cljs (glog/info logger msg))))

(defn debug [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/debug msg)
       :cljs (glog/fine logger msg))))

(defn error [& s]
  (let [msg (fmt s)]
    #?(:clj (log/error msg)
       :cljs (glog/error logger msg))))
