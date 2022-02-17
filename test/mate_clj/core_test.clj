(ns mate-clj.core-test
  (:require [clojure.test :refer :all]
            [mate-clj.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest das-test
  (testing "das->"
    (is (= (as-> 1 n
             (* 2 n)
             (+ n n)
             (+ n 2 3 4))
           (das-> 1 n
                  (* 2 n)
                  (+ n n)
                  (+ n 2 3 4))))))