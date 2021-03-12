(ns specy.number
  (:require
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.generators :as check-gen]
    #?(:cljs ["/specy/big.js" :as big :refer [Big]]);;needs shadow-cljs for properly requiring https://shadow-cljs.github.io/docs/UsersGuide.html#_requiring_js
    )
  #?(:clj (:import [java.text NumberFormat]
                   [java.math BigDecimal RoundingMode]
                   [clojure.lang BigInt])))

(defn cmp [a b]
  #?(:clj (.compareTo a b))
  #?(:cljs (.cmp a b)))

(defn greater-or-equal? [a b]
  (let [cmp-res (cmp a b)]
    (>= cmp-res 0)))

(defn ->bigint [n]
  #?(:clj (bigint n))
  #?(:cljs (big/Big n)))

(defn- scale-generator [large-integer-gen]
  (check-gen/scale #(+ 2 (/ % 20))
                   (gen/fmap (fn [xs]
                               (+ (->bigint (first xs)) (reduce * (map ->bigint (rest xs)))))
                             (gen/not-empty (gen/vector large-integer-gen)))))


(defn bigint? [n]
  #?(:clj (instance? clojure.lang.BigInt n))
  #?(:cljs (instance? big/Big n)))

(defn gen-bigint []
  (check-gen/sized
    (fn [size]
      (scale-generator (check-gen/resize size check-gen/large-integer)))))


(defn- bigint-compare-to-zero?
  [candidat comp]
  (and (bigint? candidat)
       (comp candidat (->bigint 0))))

(defn pos-bigint? [candidat]
  (bigint-compare-to-zero? candidat greater-or-equal?))

(defn gen-pos-bigint []
  (check-gen/sized
    (fn [size]
      ;; scaling gives us relatively small vectors, but using
      ;; the resized large-integer above means the numbers in
      ;; the small vectors will still be big
      (scale-generator (check-gen/resize size (check-gen/large-integer* {:min 0}))))))