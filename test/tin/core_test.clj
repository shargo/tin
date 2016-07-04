(ns tin.core-test
  (:require [clojure.test :refer :all]
            [tin.core :refer :all]
            [clojure.string :as str]))

(deftest test-assignment
  (is (= (str/trim
          (slurp "test/tin/assignment.js"))
         (str/trim
          (compile-input (slurp "test/tin/assignment.tin"))))))
