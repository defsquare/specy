(ns specy.example.amount
  "Contains a symbol with a map of currency iso code to currency value for the 20 most traded currencies in the world"
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.spec.alpha :as s]
   [specy.value :refer [defvalue]]))

(defn dissoc-unused-attrs [currency]
  (-> currency
      (dissoc :class)
      (dissoc :numericCodeAsString)))

(defn all-currencies []
  (into {}
        (map (fn [c]
               [(.getCurrencyCode c)
                (dissoc-unused-attrs (bean c))])
             (java.util.Currency/getAvailableCurrencies))))

(defn referential-data [m])

(defn currency-exists? [x]
  (get all-currencies (cond (string? x) x
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

(defvalue Currency [iso-code ::iso-code
                    numeric-code pos-int?
                    display-name string?
                    symbol string?
                    fraction-digits int?])

(defvalue Amount [qty pos-int?
                  currency Currency])

(defvalue Amount
  [qty int?
   currency Currency]
  Amountable
  (qty [amount])
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

(comment 
  (extend-type Amount
    Amountable
    (to-string [amount]
      (str qty " " (:currencyCode currency)))
    (qty [this] qty)
    (add [amount other]
      (assert-same-currency amount other)
      (->Amount (compatible-add qty (:qty other)) currency))
    (subtract [amount other]
      (assert-same-currency amount other)
      (->Amount (compatible-subtract qty (:qty other)) currency))
    (eq [this other]
       (and (.equals qty (:qty other)) (= currency (:currency other)))
                                        ;#?(:cljs (and (.eq qty (:qty other)) (= currency (:currency other))))
      )
    (currency [this] currency)
    (currency-iso-code [this] (:currencyCode currency))
    (currency-numeric-code [amount] (:numericCode currency))
    (currency-display-name [amount] (:displayName currency))
    (currency-symbol [amount] (:symbol currency))
    (currency-fraction-digits [amount] (:defaultFractionDigits currency))))

(comment 
  (defvalue {:name "Amount"
             :attrs [[:qty (s/int?)]
                     :currency Currency]
             :methods [["add" ["this" "other"]]
                       ["subtract" ["this" "other"]]]
             :protocols [Localizable]}))

