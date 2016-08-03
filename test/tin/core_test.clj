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

(deftest callFnKeyword
  (is
   (=
    (parse "foo(bar:)")
    [:program
     [:statement
      [:fncall
       [:SYMBOL "foo"]
       [:arglist
        [:KEYWORD "bar:"]]]]])))

(deftest callFn2Args
  (is
   (=
    (parse "foo(bar, baz)")
    [:program
     [:statement
      [:fncall
       [:SYMBOL "foo"]
       [:arglist
        [:SYMBOL "bar"]
        [:SYMBOL "baz"]]]]])))

(deftest callFnNoCommasFails
  (is
   (insta/failure? (parse "foo(bar baz)"))))

(deftest doubleInvokation
  (is
   (=
    (parse "foo(bar)(baz)")
    [:program
     [:statement
      [:fncall
       [:fncall
        [:SYMBOL "foo"]
        [:arglist
         [:SYMBOL "bar"]]]
       [:arglist
        [:SYMBOL "baz"]]]]])))

(deftest reassign
  (is
   (=
    (parse "a = 1")
    [:program
     [:statement
      [:SYMBOL "a"]
      [:OPERATOR "="]
      [:NUMBER "1"]]])))

(deftest keywordAssign
  (is
   (=
    (parse "a = foo:")
    [:program
     [:statement
      [:SYMBOL "a"]
      [:OPERATOR "="]
      [:KEYWORD "foo:"]]])))

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

(deftest keywordContinuation
  (is
   (=
    (parse "if a\n  b\nelif: c\n  d")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:SYMBOL "a"]
       [:block
        [:statement
         [:SYMBOL "b"]]
        [:KEYWORD "elif:"]
        [:SYMBOL "c"]
        [:statement
         [:SYMBOL "d"]]]]]])))

(deftest keywordContinuationArgs
  (is
   (=
    (parse "if a\n  b\nelse:\n  d")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:SYMBOL "a"]
       [:block
        [:statement
         [:SYMBOL "b"]]
        [:KEYWORD "else:"]
        [:statement
         [:SYMBOL "d"]]]]]])))

(deftest keywordCntinuationParens
  (is
   (=
    (parse "if (a)\n  b\nelif: (c)\n  d")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:grouping_expression
        [:SYMBOL "a"]]
       [:block
        [:statement
         [:SYMBOL "b"]]
        [:KEYWORD "elif:"]
        [:grouping_expression
         [:SYMBOL "c"]]
        [:statement
         [:SYMBOL "d"]]]]]])))

(deftest ifElifElse
  (is
   (=
    (parse "if a\n  b\nelif: c\n  d\nelse:\n  e")
    [:program
     [:statement
      [:head
       [:SYMBOL "if"]]
      [:statement_args
       [:SYMBOL "a"]
       [:block
        [:statement
         [:SYMBOL "b"]]
        [:KEYWORD "elif:"]
        [:SYMBOL "c"]
        [:statement
         [:SYMBOL "d"]]
        [:KEYWORD "else:"]
        [:statement
         [:SYMBOL "e"]]]]]])))

(deftest propertyMethodCall
  (is
   (=
    (parse "foo.bar(2)")
    [:program
     [:statement
      [:property_call
       [:SYMBOL "foo"]
       [:fncall
        [:SYMBOL "bar"]
        [:arglist
         [:NUMBER "2"]]]]]])))

(deftest propertyMethodCallMultipleArgs
  (is
   (=
    (parse "foo.bar(2, 3)")
    [:program
     [:statement
      [:property_call
       [:SYMBOL "foo"]
       [:fncall
        [:SYMBOL "bar"]
        [:arglist
         [:NUMBER "2"]
         [:NUMBER "3"]]]]]])))

(deftest propertyMultipleMethodCalls
  (is
   (=
    (parse "foo.bar(2, 3).baz(3)")
    [:program
     [:statement
      [:property_call
       [:SYMBOL "foo"]
       [:fncall
        [:SYMBOL "bar"]
        [:arglist
         [:NUMBER "2"]
         [:NUMBER "3"]]]
       [:fncall
        [:SYMBOL "baz"]
        [:arglist
         [:NUMBER "3"]]]]]])))

(deftest propertyNoArgsCalls
  (is
   (=
    (parse "foo().bar(2, 3).baz()")
    [:program
     [:statement
      [:property_call
       [:fncall
        [:SYMBOL "foo"]
        [:arglist]]
       [:fncall
        [:SYMBOL "bar"]
        [:arglist
         [:NUMBER "2"]
         [:NUMBER "3"]]]
       [:fncall
        [:SYMBOL "baz"]
        [:arglist]]]]])))

(deftest propertyAssign
  (is
   (=
    (parse "foo.bar = 12")
    [:program
     [:statement
      [:property_call
       [:SYMBOL "foo"]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

(deftest operatorPropertyCall
  (is
   (=
    (parse "(1 + 2).bar = 12")
    [:program
     [:statement
      [:property_call
       [:grouping_expression
        [:NUMBER "1"]
        [:OPERATOR "+"]
        [:NUMBER "2"]]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

(deftest propertyOperatorPrecedence
  (is
   (=
    (parse "(1 + 2).bar = 12")
    [:program
     [:statement
      [:property_call
       [:grouping_expression
        [:NUMBER "1"]
        [:OPERATOR "+"]
        [:NUMBER "2"]]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

;; (deftest test-assignment
;;   (is (= (str/trim
;;           (slurp "test/tin/assignment.js"))
;;          (str/trim
;;           (compile-input (slurp "test/tin/assignment.tin"))))))
