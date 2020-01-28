(ns specy.example.training
  (:require [specy.value :refer [defvalue]]
            [specy.entity :refer [defentity]]
            [specy.event :refer [defevent]]
            [specy.command :refer [defcommand]]
            [specy.query :refer [defquery]]
            [specy.rule :refer [defrule]]
            [specy.referential :refer [defreferential]]

            [specy.example.amount :as amount])
  (:import
   [java.time Duration]
   [specy.example.amount Amount]))

(defentity Training [id       uuid?
                     name     string?
                     content  string?
                     duration Duration
                     price    Amount])

(defn tag? [s]
  true)

(defvalue Tag
  "a tag is a string without spaces or special characters and limited in size to 42 chars"
  [tag tag?])

(defreferential Skill [id uuid?
                       name string?
                       tag Tag
                       description string?])

(defentity Instructor [id uuid? {:unique true}
                       name string?
                       skills Skill {:cardinality :multiple
                                     :queryable true}])
;(postgres/config Instructor {Skill {:data-type-gen true}})


(defrule upcoming? "True if the training has a planned date in the future" Training
  (fn [training] ))

