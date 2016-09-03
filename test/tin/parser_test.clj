(ns tin.parser-test
  (:require [clojure.test :refer :all]
            [tin.parser :refer :all]
            [tin.common :refer :all]
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
   (failure? (parse "foo(bar baz)"))))

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
       [:SYMBOL "bar"]]
      [:SYMBOL "baz"]]])))

(deftest callStrings
  (is
   (=
    (parse "foo = \"str\"\nbar(`code str`)")
    [:program
     [:operator_call
      [:SYMBOL "foo"]
      [:OPERATOR "="]
      [:STRING "\"str\""]]
     [:function_call
      [:SYMBOL "bar"]
      [:CODE_STRING "`code str`"]]])))

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
        [:SYMBOL "d"]]
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

(deftest ifStatementParensNoSpace
  (is
   (=
    (parse "if(size < 0)\n  raise ValueError(errorMessage)")
    [:program
     [:function_call
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

(deftest propertyMultipleMethodCalls
  (is
   (=
    (parse "foo.bar(2, 3).baz(3)")
    [:program
     [:function_call
      [:property_call
       [:function_call
        [:property_call
         [:SYMBOL "foo"]
         [:SYMBOL "bar"]]
        [:NUMBER "2"]
        [:NUMBER "3"]]
       [:SYMBOL "baz"]]
      [:NUMBER "3"]]])))

(deftest propertyNoArgsCalls
  (is
   (=
    (parse "foo().bar(2, 3).baz()")
    [:program
     [:function_call
      [:property_call
       [:function_call
        [:property_call
         [:function_call
          [:SYMBOL "foo"]]
         [:SYMBOL "bar"]]
        [:NUMBER "2"]
        [:NUMBER "3"]]
       [:SYMBOL "baz"]]]])))

(deftest operatorFncall
  (is
   (=
    (parse "(1 + 2).foo.bar(5)")
    [:program
     [:function_call
      [:property_call
       [:property_call
        [:operator_call
         [:NUMBER "1"]
         [:OPERATOR "+"]
         [:NUMBER "2"]]
        [:SYMBOL "foo"]]
       [:SYMBOL "bar"]]
      [:NUMBER "5"]]])))

(deftest propertyAssign
  (is
   (=
    (parse "foo.bar = 12")
    [:program
     [:operator_call
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
     [:operator_call
      [:property_call
       [:operator_call
        [:NUMBER "1"]
        [:OPERATOR "+"]
        [:NUMBER "2"]]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

(deftest propertyOperatorPrecedence
  (is
   (=
    (parse "1 + 2.bar = 12")
    [:program
     [:operator_call
      [:NUMBER "1"]
      [:OPERATOR "+"]
      [:property_call
       [:NUMBER "2"]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

(deftest parensSkipNewlines
  (is
   (=
    (parse "(1\n  +\n2).bar = 12")
    [:program
     [:operator_call
      [:property_call
       [:operator_call
        [:NUMBER "1"]
        [:OPERATOR "+"]
        [:NUMBER "2"]]
       [:SYMBOL "bar"]]
      [:OPERATOR "="]
      [:NUMBER "12"]]])))

(deftest emptyBrackets
  (is
   (=
    (parse "[]")
    [:program
     [:bracket_expression]])))

(deftest singleBrackets
  (is
   (=
    (parse "[123]")
    [:program
     [:bracket_expression
      [:NUMBER "123"]]])))

(deftest twoBrackets
  (is
   (=
    (parse "[foo, bar]")
    [:program
     [:bracket_expression
      [:SYMBOL "foo"]
      [:SYMBOL "bar"]]])))

(deftest trailingCommaBracketError
  (is
   (failure?
    (parse "[foo,]"))))

(deftest assignBrackets
  (is
   (=
    (parse "let a = [foo, bar, 123, \"str\"]")
    [:program
     [:statement_call
      [:SYMBOL "let"]
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:bracket_expression
        [:SYMBOL "foo"]
        [:SYMBOL "bar"]
        [:NUMBER "123"]
        [:STRING "\"str\""]]]]])))

(deftest fncallBrackets
  (is
   (=
    (parse "myFunction([321])")
    [:program
     [:function_call
      [:SYMBOL "myFunction"]
      [:bracket_expression
       [:NUMBER "321"]]]])))

(deftest operatorBrackets
  (is
   (=
    (parse "[321] + 789")
    [:program
     [:operator_call
      [:bracket_expression
       [:NUMBER "321"]]
      [:OPERATOR "+"]
      [:NUMBER "789"]]])))

(deftest emptyBraces
  (is
   (=
    (parse "{}")
    [:program
     [:brace_expression]])))

(deftest singleBracesFail
  (is
   (failure?
    (parse "{123}"))))

(deftest twoBrace
  (is
   (=
    (parse "{foo: bar}")
    [:program
     [:brace_expression
      [:KEYWORD "foo:"]
      [:SYMBOL "bar"]]])))

(deftest trailingCommaBraceError
  (is
   (failure?
    (parse "{foo: bar,}"))))

(deftest fourBrace
  (is
   (=
    (parse "{1: 2, three: four}")
    [:program
     [:brace_expression
      [:NUMBER "1"]
      [:NUMBER "2"]
      [:KEYWORD "three:"]
      [:SYMBOL "four"]]])))

(deftest assignBrace
  (is
   (=
    (parse "let a = {\"foo\": 2}")
    [:program
     [:statement_call
      [:SYMBOL "let"]
      [:operator_call
       [:SYMBOL "a"]
       [:OPERATOR "="]
       [:brace_expression
        [:STRING "\"foo\""]
        [:NUMBER "2"]]]]])))

(deftest fncallBraces
  (is
   (=
    (parse "myFunction({321:123})")
    [:program
     [:function_call
      [:SYMBOL "myFunction"]
      [:brace_expression
       [:NUMBER "321"]
       [:NUMBER "123"]]]])))

(deftest operatorBraces
  (is
   (=
    (parse "{321:123} + 789")
    [:program
     [:operator_call
      [:brace_expression
       [:NUMBER "321"]
       [:NUMBER "123"]]
      [:OPERATOR "+"]
      [:NUMBER "789"]]])))

(deftest unbalancedFailure
  (is
   (failure?
    (parse "(123")))
  (is
   (failure?
    (parse ")123")))
  (is
   (failure?
    (parse "{123")))
  (is
   (failure?
    (parse "{123]"))))

(deftest indexLookup
  (is
   (=
    (parse "foo[2]")
    [:program [:index_expression [:SYMBOL "foo"] [:NUMBER "2"]]])))

(deftest multipleIndexLookup
  (is
   (=
    (parse "foo[a][b]")
    [:program
     [:index_expression
      [:index_expression
       [:SYMBOL "foo"]
       [:SYMBOL "a"]]
      [:SYMBOL "b"]]])))

(deftest noIndexLookupWithSpace
  (is
   (=
    (parse "foo [2]")
    [:program
     [:statement_call
      [:SYMBOL "foo"]
      [:bracket_expression
       [:NUMBER "2"]]]])))

(deftest bracketIndexLookup
  (is
   (=
    (parse "[1][2]")
    [:program
     [:index_expression
      [:bracket_expression
       [:NUMBER "1"]]
      [:NUMBER "2"]]])))

(deftest operatorIndexLookup
  (is
   (=
    (parse "(1 + 3)[2]")
    [:program
     [:index_expression
      [:operator_call
       [:NUMBER "1"]
       [:OPERATOR "+"]
       [:NUMBER "3"]]
      [:NUMBER "2"]]])))

(deftest fncallWithBrackets
  (is
   (=
    (parse "foo([2], 3)")
    [:program
     [:function_call
      [:SYMBOL "foo"]
      [:bracket_expression
       [:NUMBER "2"]]
      [:NUMBER "3"]]])))

(deftest fncallThenIndex
  (is
   (=
    (parse "foo(3)[2]")
    [:program
     [:index_expression
      [:function_call
       [:SYMBOL "foo"]
       [:NUMBER "3"]]
      [:NUMBER "2"]]])))

(deftest indexThenFncall
  (is
   (=
    (parse "foo[2](3)")
    [:program
     [:function_call
      [:index_expression
       [:SYMBOL "foo"]
       [:NUMBER "2"]]
      [:NUMBER "3"]]])))

(deftest groupedIndex
  (is
   (=
    (parse "(foo[2])")
    [:program
     [:index_expression
      [:SYMBOL "foo"]
      [:NUMBER "2"]]])))

(deftest operatorIndexNoParens
  (is
   (=
    (parse "1 + 3[2]")
    [:program
     [:operator_call
      [:NUMBER "1"]
      [:OPERATOR "+"]
      [:index_expression
       [:NUMBER "3"]
       [:NUMBER "2"]]]])))

(deftest propertyCallIndex
  (is
   (=
    (parse "foo.bar[2]")
    [:program
     [:index_expression
      [:property_call
       [:SYMBOL "foo"]
       [:SYMBOL "bar"]]
      [:NUMBER "2"]]])))

(defest indexThenPropertyCall
  (is
   (=
    (parse "foo[3].bar")
    [:program
     [:property_call
      [:index_expression
       [:SYMBOL "foo"]
       [:NUMBER "3"]]
      [:SYMBOL "bar"]]])))

(deftest bracketsSkipNewlines
  (is
   (=
    (parse "foo[\nbar\n]")
    [:program
     [:index_expression
      [:SYMBOL "foo"]
      [:SYMBOL "bar"]]])))
