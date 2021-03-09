(ns specy.generator
  (:require [malli.generator :as mg]
            [specy.registry :as sr]))

(defn generate [building-block-id]
  (mg/generate building-block-id {:registry (sr/get-registry)}))