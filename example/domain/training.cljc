(ns domain.training
  (:require [specy.value :refer [defvalue defvalue2]]
            [specy.entity :refer [defentity defentity2]]
            [specy.event :refer [defevent]]
            [specy.command :refer [defcommand]]
            [specy.query :refer [defquery]]
            [specy.rule :refer [defrule]]
            [specy.referential :refer [defreferential defreferential2]]
            [domain.amount :refer [->amount Amount?]]
            [specy.time :refer [duration?]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [spec-tools.data-spec :as ds]
            [spec-tools.core :as st])
  (:import
    [java.time Duration LocalDateTime Period]
    [domain.amount Amount]))

(defreferential2 Tags
                 "A list of tags where tag is a string without spaces or special characters and limited to 42 chars"
                 [{:code (s/and string? #(empty? (filter (fn [c] (= \space c)) %)) #(< (count %) 42))
                   :name string?}]
                 [{:code "computer" :name "Computer science course"}
                  {:code "management" :name "Management course"}
                  {:code "english" :name "English course"}
                  {:code "french" :name "French course"}
                  {:code "practice" :name "Practice session"}
                  {:code "theory" :name "Theory course"}])

(defreferential2 Skills
                 [{:code        keyword?
                   :tags        Tags-spec
                   :description string?}]
                 [{:code        :software-development
                   :tags        [{:code "computer" :name "Computer science course"}
                                 {:code "english" :name "English course"}
                                 {:code "practice" :name "Practice session"}]
                   :description "Developing software "}])

(defentity2 Instructor "An instructor" {:id     uuid?
                                        :name   string?
                                        :skills [keyword?]})

(defentity2 Training {:id       uuid?
                      :name     string?
                      :content  string?
                      :duration duration?
                      :price    Amount?})

(s/def ::skill-id uuid?)
(s/def ::instructor-id uuid?)
(s/def ::add-skill-to-instructor-payload (s/keys :req-un [::skill-id ::instructor-id]))

(defcommand ::add-skill-to-instructor
            ::add-skill-to-instructor-payload
            {:doc     "Add a skill to an instructor"
             :rely-on Instructor})

(s/def ::find-instructors-with-skill-payload (s/keys :req-un [::skill-id]))
(defquery ::find-instructors-with-skill ::find-instructors-with-skill-payload
          {:doc     "Find an instructor with a specific skill"
           :rely-on Instructor})

(s/def ::instructors-with-skill-found-payload (s/coll-of ::instructor-id))
(defevent ::instructors-with-skill-found ::instructors-with-skill-found-payload
          {:doc     "List of instructor found with specific skill"
           :rely-on Instructor})



;(postgres/config Instructor {Skill {:data-type-gen true}})


#_(defrule upcoming? "True if the training has a planned date in the future" Training
           (fn [training]))


(comment
  (gen/generate (s/gen Skills-spec))
  (->training {:id       (java.util.UUID/randomUUID)
               :name     "Clojure in action"
               :content  "Learning Clojure for the brave !"
               :duration (Duration/between (LocalDateTime/of 2020 04 26 8 0 0) (LocalDateTime/of 2020 04 26 11 30 0))
               :price    (->amount 800 "EUR")}))