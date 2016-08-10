(ns tin.core-test
  (:require [clojure.test :refer :all]
            [tin.core :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(deftest assign
  (is
   (=
    (parse "let a = 1")
    [:program
     [:statement_call
      [:SYMBOL "let"]
      [:operator_expression
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:NUMBER "1"]]]])))

(deftest callFn
  (is
   (=
    (parse "foo(bar)")
    [:program
     [:fncall
      [:SYMBOL "foo"]
      [:arglist
       [:SYMBOL "bar"]]]])))

(deftest callFnKeyword
  (is
   (=
    (parse "foo(bar:)")
    [:program
     [:fncall
      [:SYMBOL "foo"]
      [:arglist
       [:KEYWORD "bar:"]]]])))

(deftest callFn2Args
  (is
   (=
    (parse "foo(bar, baz)")
    [:program
     [:fncall
      [:SYMBOL "foo"]
      [:arglist
       [:SYMBOL "bar"]
       [:SYMBOL "baz"]]]])))

(deftest callFnNoCommasFails
  (is
   (insta/failure? (parse "foo(bar baz)"))))

(deftest groupingExpression
  (is
   (=
    (parse "(1  + 2) * 3")
    [:program
     [:operator_expression
      [:operator_expression
       [:NUMBER "1"]
       [:OPERATOR "+"]
       [:NUMBER "2"]]
      [:OPERATOR "*"]
      [:NUMBER "3"]]])))

(deftest doubleInvokation
  (is
   (=
    (parse "foo(bar)(baz)")
    [:program
     [:fncall
      [:fncall
       [:SYMBOL "foo"]
       [:arglist
        [:SYMBOL "bar"]]]
      [:arglist
       [:SYMBOL "baz"]]]])))

(deftest reassign
  (is
   (=
    (parse "a = 1")
    [:program
     [:operator_expression
      [:SYMBOL "a"]
      [:OPERATOR "="]
      [:NUMBER "1"]]])))

(deftest keywordAssign
  (is
   (=
    (parse "a = foo:")
    [:program
     [:operator_expression
      [:SYMBOL "a"]
      [:OPERATOR "="]
      [:KEYWORD "foo:"]]])))

(deftest assignCallFn
  (is
   (=
    (parse "let a = foo(bar)")
    [:program
     [:statement_call
      [:SYMBOL "let"]
      [:operator_expression
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
     [:statement_call
      [:SYMBOL "a"]
      [:SYMBOL "b"]
      [:block
       [:statement_call
        [:SYMBOL "c"]
        [:SYMBOL "d"]]]]])))

(deftest simpleBlockParens
  (is
   (=
    (parse "a b\n  c(d)")
    [:program
     [:statement_call
      [:SYMBOL "a"]
      [:SYMBOL "b"]
      [:block
       [:fncall
        [:SYMBOL "c"]
        [:arglist
         [:SYMBOL "d"]]]]]])))

(deftest multilineBlock
  (is
   (=
    (parse "a b\n  c(d)\n  e f")
    [:program
     [:statement_call
      [:SYMBOL "a"]
      [:SYMBOL "b"]
      [:block
       [:fncall
        [:SYMBOL "c"]
        [:arglist
         [:SYMBOL "d"]]]
       [:statement_call
        [:SYMBOL "e"]
        [:SYMBOL "f"]]]]])))

(deftest ifStatement
  (is
   (=
    (parse "if size < 0\n  raise ValueError(errorMessage)")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:operator_expression
       [:SYMBOL "size"]
       [:OPERATOR "<"]
       [:NUMBER "0"]]
      [:block
       [:statement_call
        [:SYMBOL "raise"]
        [:fncall
         [:SYMBOL "ValueError"]
         [:arglist
          [:SYMBOL "errorMessage"]]]]]]])))

(deftest ifStatementParens
  (is
   (=
    (parse "if (size < 0)\n  raise ValueError(errorMessage)")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:operator_expression
       [:SYMBOL "size"]
       [:OPERATOR "<"]
       [:NUMBER "0"]]
      [:block
       [:statement_call
        [:SYMBOL "raise"]
        [:fncall
         [:SYMBOL "ValueError"]
         [:arglist
          [:SYMBOL "errorMessage"]]]]]]])))

(deftest keywordContinuation
  (is
   (=
    (parse "if a\n  b\nelif: c\n  d")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:SYMBOL "a"]
      [:block
       [:SYMBOL "b"]
       [:KEYWORD "elif:"]
       [:SYMBOL "c"]
       [:SYMBOL "d"]]]])))

(deftest keywordContinuationArgs
  (is
   (=
    (parse "if a\n  b\nelse:\n  d")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:SYMBOL "a"]
      [:block
       [:SYMBOL "b"]
       [:KEYWORD "else:"]
       [:SYMBOL "d"]]]])))

(deftest keywordCntinuationParens
  (is
   (=
    (parse "if (a + b)\n  b\nelif: (c + d)\n  d")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:operator_expression
       [:SYMBOL "a"]
       [:OPERATOR "+"]
       [:SYMBOL "b"]]
      [:block
       [:SYMBOL "b"]
       [:KEYWORD "elif:"]
       [:operator_expression
        [:SYMBOL "c"]
        [:OPERATOR "+"]
        [:SYMBOL "d"]]
       [:SYMBOL "d"]]]])))

(deftest ifElifElse
  (is
   (=
    (parse "if a\n  b\nelif: c\n  d\nelse:\n  e")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:SYMBOL "a"]
      [:block
       [:SYMBOL "b"]
       [:KEYWORD "elif:"]
       [:SYMBOL "c"]
       [:SYMBOL "d"]
       [:KEYWORD "else:"]
       [:SYMBOL "e"]]]])))

;; (deftest propertyMethodCall
;;   (is
;;    (=
;;     (parse "foo.bar(2)")
;;     [:program
;;      [:property_call
;;       [:SYMBOL "foo"]
;;       [:fncall
;;        [:SYMBOL "bar"]
;;        [:arglist
;;         [:NUMBER "2"]]]]])))

;; (deftest propertyMethodCallMultipleArgs
;;   (is
;;    (=
;;     (parse "foo.bar(2, 3)")
;;     [:program
;;      [:statement
;;       [:property_call
;;        [:SYMBOL "foo"]
;;        [:fncall
;;         [:SYMBOL "bar"]
;;         [:arglist
;;          [:NUMBER "2"]
;;          [:NUMBER "3"]]]]]])))

;; (deftest propertyMultipleMethodCalls
;;   (is
;;    (=
;;     (parse "foo.bar(2, 3).baz(3)")
;;     [:program
;;      [:statement
;;       [:property_call
;;        [:SYMBOL "foo"]
;;        [:fncall
;;         [:SYMBOL "bar"]
;;         [:arglist
;;          [:NUMBER "2"]
;;          [:NUMBER "3"]]]
;;        [:fncall
;;         [:SYMBOL "baz"]
;;         [:arglist
;;          [:NUMBER "3"]]]]]])))

;; (deftest propertyNoArgsCalls
;;   (is
;;    (=
;;     (parse "foo().bar(2, 3).baz()")
;;     [:program
;;      [:statement
;;       [:property_call
;;        [:fncall
;;         [:SYMBOL "foo"]
;;         [:arglist]]
;;        [:fncall
;;         [:SYMBOL "bar"]
;;         [:arglist
;;          [:NUMBER "2"]
;;          [:NUMBER "3"]]]
;;        [:fncall
;;         [:SYMBOL "baz"]
;;         [:arglist]]]]])))

;; (deftest propertyAssign
;;   (is
;;    (=
;;     (parse "foo.bar = 12")
;;     [:program
;;      [:statement
;;       [:operator_expression
;;        [:property_call
;;         [:SYMBOL "foo"]
;;         [:SYMBOL "bar"]]
;;        [:OPERATOR "="]
;;        [:NUMBER "12"]]]])))

;; (deftest operatorPropertyCall
;;   (is
;;    (=
;;     (parse "(1 + 2).bar = 12")
;;     [:program
;;      [:statement
;;       [:operator_expression
;;        [:property_call
;;         [:operator_expression
;;          [:NUMBER "1"]
;;          [:OPERATOR "+"]
;;          [:NUMBER "2"]]
;;         [:SYMBOL "bar"]]
;;        [:OPERATOR "="]
;;        [:NUMBER "12"]]]])))

;; (deftest propertyOperatorPrecedence
;;   (is
;;    (=
;;     (parse "1 + 2.bar = 12")
;;     [:program
;;      [:statement
;;       [:operator_expression
;;        [:NUMBER "1"]
;;        [:OPERATOR "+"]
;;        [:property_call
;;         [:NUMBER "2"]
;;         [:SYMBOL "bar"]]
;;        [:OPERATOR "="]
;;        [:NUMBER "12"]]]])))

;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
