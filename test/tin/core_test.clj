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
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:NUMBER "1"]]]])))

(deftest callFn
  (is
   (=
    (parse "foo(bar)")
    [:program
     [:function_call
      [:SYMBOL "foo"]
       [:SYMBOL "bar"]]])))

(deftest callFnKeyword
  (is
   (=
    (parse "foo(bar:)")
    [:program
     [:function_call
      [:SYMBOL "foo"]
      [:KEYWORD "bar:"]]])))

(deftest callFnNoArgs
  (is
   (=
    (parse "foo()")
    [:program
     [:function_call
      [:SYMBOL "foo"]]])))

(deftest callFn2Args
  (is
   (=
    (parse "foo(bar, baz)")
    [:program
     [:function_call
      [:SYMBOL "foo"]
      [:SYMBOL "bar"]
      [:SYMBOL "baz"]]])))

(deftest callFnOperatorExpression
  (is
   (=
    (parse "foo(2 + 2)")
    [:program
     [:function_call
      [:SYMBOL "foo"]
      [:operator_call
       [:NUMBER "2"]
       [:OPERATOR "+"]
       [:NUMBER "2"]]]])))

(deftest callFnNoCommasFails
  (is
   (insta/failure? (parse "foo(bar baz)"))))

(deftest groupingExpression
  (is
   (=
    (parse "(1 + 2) * 3")
    [:program
     [:operator_call
      [:operator_call
       [:NUMBER "1"]
       [:OPERATOR "+"]
       [:NUMBER "2"]]
      [:OPERATOR "*"]
      [:NUMBER "3"]]])))

(deftest doubleInvocation
  (is
   (=
    (parse "foo(bar)(baz)")
    [:program
     [:function_call
      [:function_call
       [:SYMBOL "foo"]
       [:SYMBOL "bar"]]]
     [:SYMBOL "baz"]])))

(deftest reassign
  (is
   (=
    (parse "a = 1")
    [:program
     [:operator_call
      [:SYMBOL "a"]
      [:OPERATOR "="]
      [:NUMBER "1"]]])))

(deftest operatorWithMethodCall
  (is
   (=
    (parse "a + b(c)")
    [:program
     [:operator_call
      [:SYMBOL "a"]
      [:OPERATOR "+"]
      [:function_call
       [:SYMBOL "b"]
       [:SYMBOL "c"]]]])))

(deftest operatorMethodCall
  (is
   (=
    (parse "(a + b)(c)")
    [:program
     [:function_call
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "+"]
       [:SYMBOL "b"]]
      [:SYMBOL "c"]]])))

(deftest keywordAssign
  (is
   (=
    (parse "a = foo:")
    [:program
     [:operator_call
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
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:function_call
        [:SYMBOL "foo"]
        [:SYMBOL "bar"]]]]])))

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
       [:function_call
        [:SYMBOL "c"]
        [:SYMBOL "d"]]]]])))

(deftest multilineBlock
  (is
   (=
    (parse "a b\n  c(d)\n  e f")
    [:program
     [:statement_call
      [:SYMBOL "a"]
      [:SYMBOL "b"]
      [:block
       [:function_call
        [:SYMBOL "c"]
        [:SYMBOL "d"]]]
      [:statement_call
       [:SYMBOL "e"]
       [:SYMBOL "f"]]]])))

(deftest ifStatement
  (is
   (=
    (parse "if size < 0\n  raise ValueError(errorMessage)")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:operator_call
       [:SYMBOL "size"]
       [:OPERATOR "<"]
       [:NUMBER "0"]]
      [:block
       [:statement_call
        [:SYMBOL "raise"]
        [:function_call
         [:SYMBOL "ValueError"]
         [:SYMBOL "errorMessage"]]]]]])))

(deftest ifStatementParens
  (is
   (=
    (parse "if (size < 0)\n  raise ValueError(errorMessage)")
    [:program
     [:statement_call
      [:SYMBOL "if"]
      [:operator_call
       [:SYMBOL "size"]
       [:OPERATOR "<"]
       [:NUMBER "0"]]
      [:block
       [:statement_call
        [:SYMBOL "raise"]
        [:function_call
         [:SYMBOL "ValueError"]
         [:SYMBOL "errorMessage"]]]]]])))

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
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "+"]
       [:SYMBOL "b"]]
      [:block
       [:SYMBOL "b"]
       [:KEYWORD "elif:"]
       [:operator_call
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

(deftest propertyMethodCall
  (is
   (=
    (parse "foo.bar(2)")
    [:program
     [:function_call
      [:property_call
       [:SYMBOL "foo"]
       [:SYMBOL "bar"]]
      [:NUMBER "2"]]])))

; (parse "(1 + 2).foo.bar(5)")

(deftest propertyMethodCallMultipleArgs
  (is
   (=
    (parse "foo.bar(2, 3)")
    [:program
     [:function_call
      [:property_call
       [:SYMBOL "foo"]
       [:SYMBOL "bar"]]
      [:NUMBER "2"]
      [:NUMBER "3"]]])))

;; (deftest propertyMultipleMethodCalls
;;   (is
;;    (=
;;     (parse "foo.bar(2, 3).baz(3)")
;;     [:program
;;      [:statement
;;       [:property_call
;;        [:SYMBOL "foo"]
;;        [:function_call
;;         [:SYMBOL "bar"]
;;
;;          [:NUMBER "2"]
;;          [:NUMBER "3"]]]
;;        [:function_call
;;         [:SYMBOL "baz"]
;;
;;          [:NUMBER "3"]]]]]])))

;; (deftest propertyNoArgsCalls
;;   (is
;;    (=
;;     (parse "foo().bar(2, 3).baz()")
;;     [:program
;;      [:statement
;;       [:property_call
;;        [:function_call
;;         [:SYMBOL "foo"]
;;         ]]
;;        [:function_call
;;         [:SYMBOL "bar"]
;;
;;          [:NUMBER "2"]
;;          [:NUMBER "3"]]]
;;        [:function_call
;;         [:SYMBOL "baz"]
;;         ]]]]])))

;; (deftest propertyAssign
;;   (is
;;    (=
;;     (parse "foo.bar = 12")
;;     [:program
;;      [:statement
;;       [:operator_call
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
;;       [:operator_call
;;        [:property_call
;;         [:operator_call
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
;;       [:operator_call
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
