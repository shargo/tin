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
     [:statement
      [:head
       [:SYMBOL "let"]]
      [:statement_args
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:NUMBER "1"]]]])))

(deftest callFn
  (is
   (=
    (parse "foo(bar)")
    [:program
     [:statement
      [:fncall
       [:SYMBOL "foo"]
       [:arglist
        [:SYMBOL "bar"]]]]])))

(deftest assignCallFn
  (is
   (=
    (parse "let a = foo(bar)")
    [:program
     [:statement
      [:head
       [:SYMBOL "let"]]
      [:statement_args
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:fncall
        [:SYMBOL "foo"]
        [:arglist
         [:SYMBOL "bar"]]]]]])))

(deftest simpleBlock
  (is
   (=
    (parse "a b\n  c d")
    [:program
     [:statement
      [:head
       [:SYMBOL "a"]]
      [:statement_args
       [:SYMBOL "b"]
       [:block
        [:statement
         [:head
          [:SYMBOL "c"]]
         [:statement_args
          [:SYMBOL "d"]]]]]]])))

(deftest simpleBlockParens
  (is
   (=
    (parse "a b\n  c(d)")
    [:program
     [:statement
      [:head
       [:SYMBOL "a"]]
      [:statement_args
       [:SYMBOL "b"]
       [:block
        [:statement
         [:fncall
          [:SYMBOL "c"]
          [:arglist
           [:SYMBOL "d"]]]]]]]])))

(deftest multilineBlock
  (is
   (=
    (parse "a b\n  c(d)\n  e f")
    [:program
     [:statement
      [:head
       [:SYMBOL "a"]]
      [:statement_args
       [:SYMBOL "b"]
       [:block
        [:statement
         [:fncall
          [:SYMBOL "c"]
          [:arglist
           [:SYMBOL "d"]]]]
        [:statement
         [:head
          [:SYMBOL "e"]]
         [:statement_args
          [:SYMBOL "f"]]]]]]])))

(deftest ifStatement
  (is
   (=
    (parse "if size < 0\n  raise ValueError(errorMessage)")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:SYMBOL "size"]
       [:OPERATOR "<"]
       [:NUMBER "0"]
       [:block
        [:statement
         [:head
          [:SYMBOL "raise"]]
         [:statement_args
          [:fncall
           [:SYMBOL "ValueError"]
           [:arglist
            [:SYMBOL "errorMessage"]]]]]]]]])))

(deftest ifStatementParens
  (is
   (=
    (parse "if (size < 0)\n  raise ValueError(errorMessage)")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:grouping_expression
        [:SYMBOL "size"]
        [:OPERATOR "<"]
        [:NUMBER "0"]]
       [:block
        [:statement
         [:head
          [:SYMBOL "raise"]]
         [:statement_args
          [:fncall
           [:SYMBOL "ValueError"]
           [:arglist
            [:SYMBOL "errorMessage"]]]]]]]]])))

;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
