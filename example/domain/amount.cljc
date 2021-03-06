(ns domain.amount
  "Contains a symbol with a map of currency iso code to currency value for the 20 most traded currencies in the world"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.generators :as check-gen]
    #?(:cljs ["/electre/common/big.js" :as big :refer [Big]]) ;;needs shadow-cljs for properly requiring https://shadow-cljs.github.io/docs/UsersGuide.html#_requiring_js
    #?(:cljs [cljs-bean.core :refer [bean ->clj ->js]])
    [specy.value :refer [defvalue]]
    [domain.currencies :refer [dissoc-unused-attrs iso->currency]]))

(defn all-currencies []
  (into {}
        (map (fn [c]
               [(.getCurrencyCode c)
                (dissoc-unused-attrs (bean c))])
             (java.util.Currency/getAvailableCurrencies))))

(defn- compatible-add [a b]
  #?(:clj (.add a b))
  #?(:cljs (.plus a b)))

(defn- compatible-subtract [a b]
  #?(:clj (.subtract a b))
  #?(:cljs (.minus a b)))

(defn- compatible-bigdec [n scale]
  #?(:clj (.setScale (bigdec n) scale java.math.BigDecimal/ROUND_HALF_UP))
  #?(:cljs (big/Big n)))

(defn bigdec? [n] #?(:clj (instance? BigDecimal n))
  #?(:cljs (instance? big/Big n)))

(defn gen-bigdec []
  (gen/fmap (fn [n]
              (compatible-bigdec n 2))
            check-gen/large-integer))

(s/check-asserts true)

(defn get-currency [iso-code]
  (get iso->currency iso-code))

(defn positive? [n] (>= n 0))
(s/def ::qty (s/with-gen (s/and bigdec? positive?)
                         gen-bigdec))

(s/def ::EUR "EUR")
(def EUR (get-currency "EUR"))
(s/def ::USD "USD")
(def USD (get-currency "USD"))

(defn currency-exists? [x]
  (get (all-currencies) (cond (string? x) x
                              (:currencyCode x) (:currencyCode x)
                              :else nil)))

(defn ^:dynamic *currencies*
  "Dynamically bind this to choose which currencies to use in generators.
  Use (binding [*currencies* (gen/return \"EUR\")] ;;your code) for instance if you want EUR only."
  []
  (gen/one-of [(gen/return (get all-currencies "EUR"))
               (s/gen (set (vals all-currencies)))]))

(s/def ::currency (s/with-gen currency-exists? *currencies*))

(s/def ::iso-code (every-pred string? #(= (count %) 3)))

(defvalue Currency [:map
                    [:iso-code [:and string? [:fn #(= (count %) 3)]]]
                    [:numeric-code pos-int?]
                    [:display-name string?]
                    [:symbol string?]
                    [:fraction-digits int?]]
          {:doc ""})

(comment 
  (defvalue Currency [iso-code ::iso-code
                      numeric-code pos-int?
                      display-name string?
                      symbol string?
                      fraction-digits int?]))


(defvalue Amount
  [qty int?
   currency Currency]
  Amountable
  (qty [this])
  (add [this other])
  (subtract [this other])
  (eq [this other])
  (currency [amount])
  (currency-iso-code [amount])
  (currency-numeric-code [amount])
  (currency-display-name [amount])
  (currency-symbol [amount])
  (currency-fraction-digits [amount]))

(defvalue Currency
  [iso-code ::iso-code]
  [numeric-code post-int?]
  [display-name string?]
  [symbol string?]
  [fraction-digits int?])

(defvalue Amount
  [qty int?]
  [currency Currency]
  Amountable
  (qty [this])
  (add [this other])
  (subtract [this other])
  (eq [this other])
  (currency [amount])
  (currency-iso-code [amount])
  (currency-numeric-code [amount])
  (currency-display-name [amount])
  (currency-symbol [amount])
  (currency-fraction-digits [amount]))

;; => constructor, protocol

(defn assert-same-currency [amount other]
  (assert (= (currency amount) (currency other))
          (str "Amount have two different currencies (should be the same): " (currency amount)
               " and " (currency other))))

(defvalue Amount
          [:map
           [:qty int?]
           [:currency [:fn Currency?]]]
          {:doc ""}
          (fn to-string [this] (str (:qty this) " " (:currencyCode currency)))
          (fn qty [this] (:qty this))
          (fn add [this other]
            (assert-same-currency this other)
            (->Amount (compatible-add (:qty this) (:qty other)) (:currency this)))
          (fn subtract [this other]
            (assert-same-currency this other)
            (->Amount (compatible-subtract (:qty this) (:qty other)) (:currency this)))
          (fn eq [this other]
            (and (.equals (:qty this) (:qty other)) (= (:currency this) (:currency other)))
            ;#?(:cljs (and (.eq qty (:qty other)) (= currency (:currency other))))
            )
          (fn currency [this] (:currency this))
          (fn currency-iso-code [this] (:currencyCode (:currency this)))
          (fn currency-numeric-code [this] (:numericCode (:currency this)))
          (fn currency-display-name [this] (:displayName (:currency this)))
          (fn currency-symbol [this] (:symbol (:currency this)))
          (fn currency-fraction-digits [this] (:defaultFractionDigits (:currency this))))

(defn- currency-from-arg [x]
  (when x
    (if (string? x)
      (do (s/assert currency-exists? x)
          (get-currency x))
      #?(:clj (if (instance? Currency x) (dissoc-unused-attrs (bean x)) x))
      #?(:cljs x))))

(defn ->amount-builder [qty currency-arg]
  (let [currency (currency-from-arg currency-arg)
        qty-bigdec (compatible-bigdec qty (or (:defaultFractionDigits currency) 2))]
    (s/assert ::qty qty-bigdec)
    (assert currency "Currency cannot be nil")
    (->Amount qty-bigdec currency)))

