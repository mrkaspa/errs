(ns errs.core-test
  (:require [clojure.test :refer [deftest is]]
            [errs.core :refer [ok error if-ok try-ok ok-> ok->>]]))

(deftest test-if-ok
  (testing "When exception"
    (let [res (-> (ok 1)
                  (if-ok (try-ok #(/ % 0)))
                  (if-ok (fn [n] (ok (+ 100 n)))))]
      (is (= res (error "Divide by zero"))))
    (testing "When all ok"
      (let [res (-> (ok 1)
                    (if-ok (try-ok #(+ % 1)))
                    (if-ok (fn [n] (ok (+ 100 n)))))]
        (is (= res (ok 102)))))))

(deftest test-ok->
  (testing "When exception"
    (let [res (ok-> 1
                    inc
                    #(/ % 0))]
      (is (= res (error "Divide by zero")))))
  (testing "When all ok"
    (let [res (ok-> 1
                    inc
                    inc)]
      (is (= res (ok 3))))))

(deftest test-ok->>
  (testing "When exception"
    (let [res (ok->> 1
                     inc
                     #(/ % 0))]
      (is (= res (error "Divide by zero")))))
  (testing "When all ok"
    (let [res (ok->> 1
                     inc
                     inc)]
      (is (= res (ok 3))))))
