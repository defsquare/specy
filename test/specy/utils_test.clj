(ns specy.utils-test
  (:require  [clojure.test :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.test.check.generators :as check-gen]
             [testit.core :refer :all]
             [specy.utils :refer :all]
             [specy.infra.repository :refer [->BuildingBlocksRepositoryInMemory]]
             [clojure.string :as string]))

;sample value type as a defrecord and spec keyword
(defrecord Skill [a b c])

(s/def ::qty (s/with-gen nat-int?
               check-gen/int))

(def building-blocks (->BuildingBlocksRepositoryInMemory (atom {}) (atom {})))

(defmacro fields-test
  [fields assert-data]
  (t/deftest fields-tests-decl
    (t/testing "fields inspection"
      (let [inspected-fields (inspect building-blocks fields)
            ns *ns*]
        (do
             (doseq [field inspected-fields]
               (t/is ((:field-names assert-data) (:field-name field)))
               (t/is ((:x assert-data) (:ref field)))))))))

(do
  ;;needs to wrap deftest in a macro to test the symbol given to 
  (fields-test [id uuid? {:unique true}
                name string?
                duration java.time.Duration
                description string?
                skills Skill {:cardinality :multiple
                              :queryable true}]
               {:field-names #{"id" "duration" "description" "name" "skills"}
                :x           #{uuid? java.time.Duration string? Skill}})

  (t/run-tests 'specy.utils-test))
