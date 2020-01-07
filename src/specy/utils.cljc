(ns specy.utils
  (:require [#?(:clj  clojure.spec.gen.alpha
                :cljs cljs.spec.gen.alpha) :as gen]
            [#?(:clj  clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            ))

(defn pred-spec-or-class? [x]
  (cond
    (and (keyword? x) (s/get-spec x)) :spec
    (var? (resolve x)) (when-let [fn? (fn? (deref (resolve x)))]
                         (when fn? (let [argslist (:arglists (meta #'int?))]
                                     :pred)))
    (class? (type (resolve x))) :class
    :default (throw (ex-info "pred spec or class not found" {:x x}))))

(defn inspect [fields]
  (let [m (apply hash-map fields)]
    (map (fn [[field-name x]]
           {:field field-name
            :field-name (str field-name)
            :ref x
            :kind (pred-spec-or-class? x)}) m)))


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

(defn- parse-opts+specs [opts+specs]
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
