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

(defmacro dcond->>
  [expr & clauses]
  (assert (even? (count clauses)))
  (loop [x expr, clauses clauses]
    (if clauses
      (let [test (first clauses)
            step (second clauses)
            pass-test (eval test)
            threaded (if pass-test
                       (if (seq? step)
                         (with-meta `(~(first step) ~@(next step) ~x) (meta step))
                         (list step x))
                       x)]
        (when pass-test (println threaded "=>" (eval threaded)))
        (recur threaded (next (next clauses))))
      x)))

(defmacro das->
  [expr name & clauses]
  (let [c (gensym) s (gensym) t (gensym)]
    `(let [~name ~expr]
       (println '~name "=>" ~expr)
       (loop [~name ~expr, ~c '~clauses]
         (if ~c
           (let [~s (first ~c)
                 ~t (if (seq? ~s)
                      (with-meta `(~(first ~s) ~@(next (map #(if (= % '~name) ~name %) ~s))) (meta ~s))
                      (if
                        (= ~s '~name)
                        ~name
                        ~s))]
             (println ~s "=>" (eval ~t))
             (recur (eval ~t) (next ~c)))
          ~name)))))

(defn dfilter
  ([pred]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (println pred input "=> " (pred input))
         (if (pred input)
           (rf result input)
           result)))))
  ([pred coll]
     (when-let [s (seq coll)]
       (if (chunked-seq? s)
         (let [c (chunk-first s)
               size (count c)
               b (chunk-buffer size)]
           (dotimes [i size]
             (let [v (.nth c i)]
               (println pred v "=> " (pred v))
               (when (pred v)
                 (chunk-append b v))))
           (chunk-cons (chunk b) (dfilter pred (chunk-rest s))))
         (let [f (first s) r (rest s)]
           (println pred f "=> " (pred f))
           (if (pred f)
             (cons f (dfilter pred r))
             (dfilter pred r)))))))

(defn dremove
  ([pred] (dfilter (complement pred)))
  ([pred coll]
     (dfilter (complement pred) coll)))


(comment
  (def xf (dfilter odd?))
  (transduce xf conj (range 10))

  (dfilter even? [1 2 3 4 5 6])

  (dfilter (fn [x]
             (= (count x) 1))
           ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""])

  (dfilter #(> (second %) 100)
           {:a 1
            :b 2
            :c 101
            :d 102
            :e -1}))