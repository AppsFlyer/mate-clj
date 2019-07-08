(ns mate-clj.core)

(defmacro d->
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (println threaded "=>" (eval threaded))
        (recur threaded (next forms)))
      x)))

(defmacro d->>
   [x & forms]
   (loop [x x, forms forms]
     (if forms
       (let [form (first forms)
             threaded (if (seq? form)
                        (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                        (list form x))]
         (println threaded "=>" (eval threaded))
         (recur threaded (next forms)))
       x)))

(defmacro dsome-> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (let [expr (eval threaded)]
          (println threaded "=>" expr)
          (if-not (nil? expr)
            (recur threaded (next forms))
            nil)))
      x)))

(defmacro dsome->> [x & forms]
  (loop [x x, forms forms]
     (if forms
       (let [form (first forms)
             threaded (if (seq? form)
                        (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                        (list form x))]
         (let [expr (eval threaded)]
           (println threaded "=>" expr)
           (if-not (nil? expr)
             (recur threaded (next forms))
             nil)))
       x)))

(defn dsome
  [pred coll]
  (when (seq coll)
    (or (do
          (println `(~pred ~(first coll)) "=>" (pred (first coll)))
          (pred (first coll))) (recur pred (next coll)))))

(defn dreduce
  ([f coll]
   (if (seq coll)
     (dreduce f (first coll) (rest coll))
     (f)))
  ([f val coll]
   (if (seq coll)
     (do
       (println `(~f ~val ~(first coll)) "=>" (f val (first coll)))
       (recur f (f val (first coll)) (rest coll)))
     val)))

(defmacro dcond->
  [expr & clauses]
  (assert (even? (count clauses)))
  (loop [x expr, clauses clauses]
    (if clauses
      (let [test (first clauses)
            step (second clauses)
            pass-test (eval test)
            threaded (if pass-test
                       (if (seq? step)
                         (with-meta `(~(first step) ~x ~@(next step)) (meta step))
                         (list step x))
                       x)]
        (when pass-test (println threaded "=>" (eval threaded)))
        (recur threaded (next (next clauses))))
      x)))

(comment
  (dcond-> 1
           true inc
           (= 3 2) (* 42)
           true (+ 100)
           (= 2 2) (* 9))
)