(ns specy.utils
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [specy.protocols :refer [find-value-by-class]]
            ))

(declare create-map)
(defmacro create-map
  "Utility macro that create a map from a list of symbols that are in scope"
  [& syms]
  (zipmap (map keyword syms) syms))

(defn pred-spec-value-or-class?
  ([x]
   (pred-spec-value-or-class? x))
  ([building-blocks-repo x]
   (cond
     (and (keyword? x) (s/get-spec x)) :spec
     (var? (resolve x)) (when-let [fn? (fn? (deref (resolve x)))]
                          (when fn? (let [argslist (:arglists (meta #'int?))]
                                      :pred)))
     (when building-blocks-repo (find-value-by-class building-blocks-repo x)) :value
     (class? (type (resolve x))) :class
     :default (throw (ex-info (str "Pred, spec, value or class "x" not found! " x " must be a predicate function, a spec keyword, a value or a class type") {:x x})))))

(defn inspect [building-blocks-repo fields]
  (loop [[field-name x opts? & remainings-fields] fields
         acc []]
    (let [kind (pred-spec-value-or-class? building-blocks-repo x)
          ;if opts? is a coll then it is considered present, otherwise it's a symbol representing the next field name that must be kept for the next iteration
          remainings-fields (if (coll? opts?) remainings-fields (cons opts? remainings-fields))
          acc (conj acc (merge {:field field-name
                                :field-name (str field-name)
                                :ref x
                                :kind kind} (when (coll? opts?) {:opts opts?})))]
      (if (empty? (filter identity remainings-fields));filter nil in remainings-fields
        acc
        (recur remainings-fields
               acc)))))


(defn- parse-opts [s]
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn- maybe-destructured [params]
  (if (every? symbol? params)
    params
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          ~lets)))))

(defn parse-opts+specs [opts+specs]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (-> (map #(if (var? (resolve %))
                               (:on (deref (resolve %)))
                               %)
                            (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        methods (map (fn [[name params]]
                       (cons name (maybe-destructured params)))
                     (apply concat (vals impls)))]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    [interfaces methods opts]))

(defn operations [methods]
  (vec (map (fn [[name & params]]
              {:operation-name (str name)
               :operation-fn (resolve name)
               :params  (vec (map (fn [param] (str param)) params))}) methods)))
