(ns specy.registry
  (:require [clojure.test.check.generators :as clj-gen]
            [malli.core :as mc]
            [malli.generator :as mg]
            [specy.number :as n]))


(def -specy-registry* (atom (mc/default-schemas)))

(defn get-registry [] (deref -specy-registry*))

(defn clean [] (reset! -specy-registry* (mc/default-schemas)))

(defn register!
  ([schema-name pred schema-or-gen]
   (register! schema-name pred schema-or-gen nil))
  ([schema-name pred schema-or-gen error-msg]
   (swap! -specy-registry* assoc schema-name (mc/-simple-schema (cond-> {:type schema-name :pred pred }
                                                                        error-msg (assoc :type-properties {:error/message error-msg}))))
   (defmethod mg/-schema-generator schema-name [_ options] (if (clj-gen/generator? schema-or-gen)
                                                             schema-or-gen
                                                             (mg/generator schema-or-gen {:registry @-specy-registry*})))))

(register! :bigint n/bigint? (n/gen-bigint))
(register! :pos-bigint n/pos-bigint? (n/gen-pos-bigint))
