(ns specy.example.training
  (:require [specy.value :refer [defvalue]]
            [specy.entity :refer [defentity]]
            [specy.event :refer [defevent]]
            [specy.command :refer [defcommand]]
            [specy.query :refer [defquery]]

            [specy.example.amount :as amount])
  (:import
   [java.time Duration]
   [specy.example.amount Amount]))

(defentity Training
  [id uuid?
   name string?
   content string?
   duration Duration
   price Amount])


