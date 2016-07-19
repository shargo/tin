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

(deftest simpleBlock
  (is
   (=
    (parse "a b\n  c d")
    [:program
     [:fncall
      [:function_expression [:symbol "a"]]
      [:function_arguments
       [:symbol "b"]
       [:block [:fncall
                [:function_expression [:symbol "c"]]
                [:function_arguments [:symbol "d"]]]]]]])))

(deftest simpleBlockParens
  (is
   (=
    (parse "a b\n  c(d)")
    [:program
     [:fncall
      [:function_expression [:symbol "a"]]
      [:function_arguments
       [:symbol "b"]
       [:block [:fncall
                [:function_expression [:symbol "c"]]
                [:function_arguments [:symbol "d"]]]]]]])))

(deftest multilineBlock
  (is
   (=
    (parse "a b\n  c(d)\n  e f")
    [:program
     [:fncall
      [:function_expression [:symbol "a"]]
      [:function_arguments
       [:symbol "b"]
       [:block
        [:fncall
         [:function_expression [:symbol "c"]]
         [:function_arguments [:symbol "d"]]]
        [:fncall
         [:function_expression [:symbol "e"]]
         [:function_arguments [:symbol "f"]]]]]]])))

(deftest ifStatement
  (is
   (=
    (parse "if size < 0\n  raise ValueError(errorMessage)")
    [:program
     [:fncall
      [:function_expression [:symbol "if"]]
      [:function_arguments [:symbol "size"] [:symbol "<"] [:number "0"]
       [:block
        [:fncall
         [:function_expression [:symbol "raise"]]
         [:function_arguments
          [:fncall
           [:function_expression [:symbol "ValueError"]]
           [:function_arguments [:symbol "errorMessage"]]]]]]]]])))

;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
