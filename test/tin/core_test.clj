(ns tin.core-test
  (:require [clojure.test :refer :all]
            [tin.core :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(deftest assign
  (is
   (=
    (parse "let a = 1")
    [:program
     [:fncall
      [:function_expression [:symbol "let"]]
      [:function_arguments [:symbol "a"] [:symbol "="] [:number "1"]]]])))

(deftest callFn
  (is
   (=
    (parse "foo(bar)")
    [:program [:fncall [:function_expression [:symbol "foo"]]
               [:function_arguments [:symbol "bar"]]]])))

(deftest assignCallFn
  (is
   (=
    (parse "let a = foo(bar)")
    [:program [:fncall [:function_expression [:symbol "let"]]
               [:function_arguments [:symbol "a"] [:symbol "="]]]
     [:fncall [:function_expression [:symbol "foo"]]
      [:function_arguments [:symbol "bar"]]]])))

;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
