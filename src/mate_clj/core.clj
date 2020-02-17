(ns mate-clj.core)

(def ^:private ^:const arrow-str "=>")

(defmacro d->
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (println threaded arrow-str (eval threaded))
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
         (println threaded arrow-str (eval threaded))
         (recur threaded (next forms)))
       x)))

(defmacro dsome-> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))
            expr (eval threaded)]
          (println threaded arrow-str expr)
          (if-not (nil? expr)
            (recur threaded (next forms))
            nil))
      x)))

(defmacro dsome->> [x & forms]
  (loop [x x, forms forms]
     (if forms
       (let [form (first forms)
             threaded (if (seq? form)
                        (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                        (list form x))
             expr (eval threaded)]
           (println threaded arrow-str expr)
           (if-not (nil? expr)
             (recur threaded (next forms))
             nil))
       x)))

(defn dsome
  [pred coll]
  (when (seq coll)
    (or (do
          (println `(~pred ~(first coll)) arrow-str (pred (first coll)))
          (pred (first coll))) (recur pred (next coll)))))

(defn dreduce
  ([f coll]
   (if (seq coll)
     (dreduce f (first coll) (rest coll))
     (f)))
  ([f val coll]
   (if (seq coll)
     (do
       (println `(~f ~val ~(first coll)) arrow-str (f val (first coll)))
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
        (when pass-test (println threaded arrow-str (eval threaded)))
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
        (when pass-test (println threaded arrow-str (eval threaded)))
        (recur threaded (next (next clauses))))
      x)))

(defmacro das->
  [expr name & clauses]
  (let [c (gensym) s (gensym) t (gensym)]
    `(let [~name ~expr]
       (println '~name arrow-str ~expr)
       (loop [~name ~expr, ~c '~clauses]
         (if ~c
           (let [~s (first ~c)
                 ~t (if (seq? ~s)
                      (with-meta `(~(first ~s) ~@(next (map #(if (= % '~name) ~name %) ~s))) (meta ~s))
                      (if
                        (= ~s '~name)
                        ~name
                        ~s))]
             (println ~s arrow-str (eval ~t))
             (recur (eval ~t) (next ~c)))
          ~name)))))

(defn dfilter
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (println pred input arrow-str (pred input))
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
             (println pred v arrow-str (pred v))
             (when (pred v)
               (chunk-append b v))))
         (chunk-cons (chunk b) (dfilter pred (chunk-rest s))))
       (let [f (first s) r (rest s)]
         (println pred f arrow-str (pred f))
         (if (pred f)
           (cons f (dfilter pred r))
           (dfilter pred r)))))))

(defn dremove
  ([pred] (dfilter (complement pred)))
  ([pred coll]
   (dfilter (complement pred) coll)))

(defn dtake-while
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (println pred input arrow-str (pred input))
        (if (pred input)
          (rf result input)
          (reduced result))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (println pred (first s) arrow-str (pred (first s)))
      (when (pred (first s))
        (cons (first s) (dtake-while pred (rest s))))))))

(defn ddrop-while
  ([pred]
   (fn [rf]
     (let [dv (volatile! true)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [drop? @dv]
            (println pred input arrow-str (pred input))
            (if (and drop? (pred input))
              result
              (do
                (vreset! dv nil)
                (rf result input)))))))))
  ([pred coll]
   (let [step (fn [pred coll]
                (let [s (seq coll)]
                  (println pred (first s) arrow-str (pred (first s)))
                  (if (and s (pred (first s)))
                    (recur pred (rest s))
                    s)))]
     (lazy-seq (step pred coll)))))

(defn dsplit-with
  [pred coll]
  [(dtake-while pred coll) (ddrop-while pred coll)])

(defmacro dwhile
  [test & body]
  `(loop []
     (println '~test arrow-str ~test)
     (when ~test
       ~@body
       (recur))))

(defn dkeep
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [v (f input)]
          (println f input arrow-str v)
          (if (nil? v)
            result
            (rf result v)))))))
  ([f coll]
   (when-let [s (seq coll)]
     (if (chunked-seq? s)
       (let [c (chunk-first s)
             size (count c)
             b (chunk-buffer size)]
         (dotimes [i size]
           (let [x (f (.nth c i))]
             (println f (.nth c i) arrow-str x)
             (when-not (nil? x)
               (chunk-append b x))))
         (chunk-cons (chunk b) (keep f (chunk-rest s))))
       (let [x (f (first s))]
         (println f (first s) arrow-str x)
         (if (nil? x)
           (dkeep f (rest s))
           (cons x (dkeep f (rest s)))))))))

(defn dkeep-indexed
  ([f]
   (fn [rf]
     (let [iv (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [i (vswap! iv inc)
                v (f i input)]
            (println f i input arrow-str v)
            (if (nil? v)
              result
              (rf result v))))))))
  ([f coll]
   (letfn [(keepi [idx coll]
             (when-let [s (seq coll)]
               (if (chunked-seq? s)
                 (let [c (chunk-first s)
                       size (count c)
                       b (chunk-buffer size)]
                   (dotimes [i size]
                     (let [ind (+ idx i)
                           nth-val (.nth c i)
                           x (f ind nth-val)]
                       (println f ind nth-val arrow-str x)
                       (when-not (nil? x)
                         (chunk-append b x))))
                   (chunk-cons (chunk b) (keepi (+ idx size) (chunk-rest s))))
                 (let [x (f idx (first s))]
                   (println f idx (first s) arrow-str x)
                   (if (nil? x)
                     (keepi (inc idx) (rest s))
                     (cons x (keepi (inc idx) (rest s))))))))]
     (keepi 0 coll))))

