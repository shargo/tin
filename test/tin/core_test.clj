(ns tin.core-test
  (:require [clojure.test :refer :all]
            [tin.core :refer :all]
            [clojure.string :as str]))

(deftest test-parsing
  (is (=
       [:program [:expression [:symbol "a"] [:symbol "="] [:number "1"]]]
       (parse "(a = 1)")
       ))
  (is (=
       [:program [:expression [:symbol "+"] [:number "1"] [:number "2"]]]
       (parse "(+ 1 2)")
       ))
  )

(deftest test-assignment
  (is (= (str/trim
          (slurp "test/tin/assignment.js"))
         (str/trim
          (compile-input (slurp "test/tin/assignment.tin"))))))
