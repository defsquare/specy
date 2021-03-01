(ns specy.validation
  (:require [malli.core :as mc]
            [malli.error :as me]
            [specy.internal.logging :as log]))

(defn assert-schema [schema data]
  (if (mc/validate schema data)
    data
    (let [explain (mc/explain schema data)]
      (throw (ex-info (str "Not conform to schema :\n" (me/humanize explain) "")
                      explain)))))

(defn valid? [schema data] (try (any? (assert-schema schema data))
                                       #?(:clj  (catch Exception e (log/error (str (.getMessage e)  " - " (.getData e))) false)
                                          :cljs (catch :default e (log/error e) false))))