(ns domain.training
  (:require [specy.value :refer [defvalue]]
            [specy.entity :refer [defentity]]
            [specy.event :refer [defevent]]
            [specy.command :refer [defcommand]]
            [specy.query :refer [defquery]]
            [specy.rule :refer [defrule]]
            [specy.referential :refer [defreferential]]
            [clojure.spec.alpha :as s])
  (:import
   [java.time Duration]
   [domain.amount Amount]))

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

(s/def ::skill-id uuid?)
(s/def ::instructor-id uuid?)
(s/def ::add-skill-to-instructor-payload (s/keys :req-un [::skill-id ::instructor-id]))

(defcommand ::add-skill-to-instructor
            ::add-skill-to-instructor-payload
            {:doc "Add a skill to an instructor"
             :rely-on Instructor})

(s/def ::find-instructors-with-skill-payload (s/keys :req-un [::skill-id]))
(defquery ::find-instructors-with-skill ::find-instructors-with-skill-payload
          {:doc "Find an instructor with a specific skill"
           :rely-on Instructor})

(s/def ::instructors-with-skill-found-payload (s/coll-of ::instructor-id))
(defevent ::instructors-with-skill-found ::instructors-with-skill-found-payload
          {:doc "List of instructor found with specific skill"
           :rely-on Instructor})


;(postgres/config Instructor {Skill {:data-type-gen true}})


(defrule upcoming? "True if the training has a planned date in the future" Training
  (fn [training] ))

