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
                       (list form x))
            expr (eval threaded)]
          (println threaded "=>" expr)
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
           (println threaded "=>" expr)
           (if-not (nil? expr)
             (recur threaded (next forms))
             nil))
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
        (println pred input "=>" (pred input))
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
             (println pred v "=>" (pred v))
             (when (pred v)
               (chunk-append b v))))
         (chunk-cons (chunk b) (dfilter pred (chunk-rest s))))
       (let [f (first s) r (rest s)]
         (println pred f "=>" (pred f))
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
        (println pred input "=>" (pred input))
        (if (pred input)
          (rf result input)
          (reduced result))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (println pred (first s) "=>" (pred (first s)))
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
            (println pred input "=>" (pred input))
            (if (and drop? (pred input))
              result
              (do
                (vreset! dv nil)
                (rf result input)))))))))
  ([pred coll]
   (let [step (fn [pred coll]
                (let [s (seq coll)]
                  (println pred (first s) "=>" (pred (first s)))
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
     (println '~test "=>" ~test)
     (when ~test
       ~@body
       (recur))))

(defn devery?
  [pred coll]
  (cond
    (nil? (seq coll)) true
    (pred (first coll))
    (do
      (println pred (first coll) "=>" (pred (first coll)))
      (recur pred (next coll)))
    :else
    (do
      (println pred (first coll) "=> false")
      false)))

:(defn devery-pred
  ([p]
   (fn ep1
     ([] true)
     ([x]
      (let [r-p-x (boolean (p x))]
        (println p x "=>" r-p-x) r-p-x))
     ([x y]
      (let [r-p-x (boolean (p x))
            r-p-y (boolean (p y))]
        (println p x "=>" r-p-x)
        (println p y "=>" r-p-y)
        (and r-p-x r-p-y)))
     ([x y z]
      (let [r-p-x (boolean (p x))
            r-p-y (boolean (p y))
            r-p-z (boolean (p z))]
        (println p x "=>" r-p-x)
        (println p y "=>" r-p-y)
        (println p z "=>" r-p-z)
        (and r-p-x r-p-y r-p-z)))
     ([x y z & args] (boolean (and (ep1 x y z)
                                   (devery? p args))))))
  ([p1 p2]
   (fn ep2
     ([] true)
     ([x]
      (let [r-p1-x (boolean (p1 x))
            r-p2-x (boolean (p2 x))]
        (println p1 x "=>" r-p1-x)
        (println p2 x "=>" r-p2-x)
        (and r-p1-x r-p2-x)))
     ([x y]
      (let [r-p1-x (boolean (p1 x))
            r-p1-y (boolean (p1 y))
            r-p2-x (boolean (p2 x))
            r-p2-y (boolean (p2 y))]
        (println p1 x "=>" r-p1-x)
        (println p1 y "=>" r-p1-y)
        (println p2 x "=>" r-p2-x)
        (println p2 y "=>" r-p2-y)
        (and r-p1-x r-p1-y r-p2-x r-p2-y)))
     ([x y z]
      (let [r-p1-x (boolean (p1 x))
            r-p1-y (boolean (p1 y))
            r-p1-z (boolean (p1 z))
            r-p2-x (boolean (p2 x))
            r-p2-y (boolean (p2 y))
            r-p2-z (boolean (p2 z))]
        (println p1 x "=>" r-p1-x)
        (println p1 y "=>" r-p1-y)
        (println p1 z "=>" r-p1-z)
        (println p2 x "=>" r-p2-x)
        (println p2 y "=>" r-p2-y)
        (println p2 z "=>" r-p2-z)
        (and r-p1-x r-p1-y r-p1-z r-p2-x r-p2-y r-p2-z)))
     ([x y z & args] (boolean (and (ep2 x y z)
                                   (devery? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
   (fn ep3
     ([] true)
     ([x]
      (let [r-p1-x (boolean (p1 x))
            r-p2-x (boolean (p2 x))
            r-p3-x (boolean (p3 x))]
        (println p1 x "=>" r-p1-x)
        (println p2 x "=>" r-p2-x)
        (println p3 x "=>" r-p3-x)
        (and r-p1-x r-p2-x r-p3-x)))
     ([x y]
      (let [r-p1-x (boolean (p1 x))
            r-p2-x (boolean (p2 x))
            r-p3-x (boolean (p3 x))
            r-p1-y (boolean (p1 y))
            r-p2-y (boolean (p2 y))
            r-p3-y (boolean (p3 y))]
        (println p1 x "=>" r-p1-x)
        (println p2 x "=>" r-p2-x)
        (println p3 x "=>" r-p3-x)
        (println p1 y "=>" r-p1-y)
        (println p2 y "=>" r-p2-y)
        (println p3 y "=>" r-p3-y)
        (and r-p1-x r-p2-x r-p3-x r-p1-y r-p2-y r-p3-y)))
     ([x y z]
      (let [r-p1-x (boolean (p1 x))
            r-p2-x (boolean (p2 x))
            r-p3-x (boolean (p3 x))
            r-p1-y (boolean (p1 y))
            r-p2-y (boolean (p2 y))
            r-p3-y (boolean (p3 y))
            r-p1-z (boolean (p1 z))
            r-p2-z (boolean (p2 z))
            r-p3-z (boolean (p3 z))]
        (println p1 x "=>" r-p1-x)
        (println p2 x "=>" r-p2-x)
        (println p3 x "=>" r-p3-x)
        (println p1 y "=>" r-p1-y)
        (println p2 y "=>" r-p2-y)
        (println p3 y "=>" r-p3-y)
        (println p1 z "=>" r-p1-z)
        (println p2 z "=>" r-p2-z)
        (println p3 z "=>" r-p3-z)
        (and r-p1-x r-p2-x r-p3-x r-p1-y r-p2-y r-p3-y r-p1-z r-p2-z r-p3-z)))
     ([x y z & args] (boolean (and (ep3 x y z)
                                   (devery? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
   (let [ps (list* p1 p2 p3 ps)]
     (fn epn
       ([] true)
       ([x] (devery? #(% x) ps))
       ([x y] (devery? #(and (% x) (% y)) ps))
       ([x y z] (devery? #(and (% x) (% y) (% z)) ps))
       ([x y z & args] (boolean (and (epn x y z)
                                     (devery? #(devery? % args) ps))))))))

