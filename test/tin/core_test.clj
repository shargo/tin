(ns tin.core-test
  (:require [clojure.test :refer :all]
            [tin.core :refer :all]
            [clojure.string :as str]))

(defmacro test-parse-equals
  [string program]
  `(deftest ~(gensym "test")
     (is (= (parse ~string) ~program))))

(test-parse-equals
 "let a = 1"
 [:program [:fncall [:function_expression [:symbol "let"]]
            [:function_arguments [:symbol "a"] [:symbol "="] [:number "1"]]]])

(test-parse-equals
 "foo(bar)"
 [:program [:fncall [:function_expression [:symbol "foo"]]
            [:function_arguments [:symbol "bar"]]]])
;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
