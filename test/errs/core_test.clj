(ns errs.core-test
  (:require [clojure.test :refer :all]
            [errs.core :refer :all]))

(deftest if-ok
  (testing "When an error comes"
    (let [res (-> (ok 1)
                  (if-ok (try-ok #(ok (/ % 0))))
                  (if-ok (fn [n] (ok (+ 100 n)))))]
      (is (= res [:error "Divide by zero"]))))
  (testing "When all ok"
    (let [res (-> (ok 1)
                  (if-ok (try-ok #(ok (+ % 1))))
                  (if-ok (fn [n] (ok (+ 100 n)))))]
      (is (= res [:ok 102])))))
