(ns tin.lexer-test
  (:require [clojure.test :refer :all]
            [tin.lexer :refer :all]
            [tin.common :refer :all]))

(deftest simpleMethodCall
  (is
   (=
    (tin.lexer/tokenize-string "foo.bar(2)")
    ["LINE_START" "SYMBOL(foo)" "DOT" "SYMBOL(bar)" "LPAREN" "NUMBER(2)"
     "RPAREN" "LINE_START"])))

(deftest significantWhitespace
  (is
   (=
    (tin.lexer/tokenize-string "foo.bar (2)")
    ["LINE_START" "SYMBOL(foo)" "DOT" "SYMBOL(bar)" "WS" "LPAREN" "NUMBER(2)"
     "RPAREN" "LINE_START"])))

(deftest initialNewline
  (is
   (=
    (tin.lexer/tokenize-string "\nfoo")
    ["LINE_START" "SYMBOL(foo)" "LINE_START"])))

(deftest initialIndent
  (is
   (=
    (tin.lexer/tokenize-string "  foo")
    ["INDENT" "LINE_START" "SYMBOL(foo)" "DEDENT" "LINE_START"])))

(deftest whitespaceLine
  (is
   (=
    (tin.lexer/tokenize-string "foo\n     \nbar")
    ["LINE_START" "SYMBOL(foo)" "LINE_START" "SYMBOL(bar)" "LINE_START"])))

(deftest commentLine
  (is
   (=
    (tin.lexer/tokenize-string "foo\n  // Comment\nbar")
    ["LINE_START" "SYMBOL(foo)" "LINE_START" "SYMBOL(bar)" "LINE_START"])))

(deftest blockCommentLine
  (is
   (=
    (tin.lexer/tokenize-string "foo\n  /*Block Comment*/\nbar")
    ["LINE_START" "SYMBOL(foo)" "LINE_START" "SYMBOL(bar)" "LINE_START"])))

(deftest finalNewline
  (is
   (=
    (tin.lexer/tokenize-string "foo\n\n")
    ["LINE_START" "SYMBOL(foo)" "LINE_START"])))

(deftest illegalColon
  (is
   (failure?
    (tin.lexer/tokenize-string "foo : bar"))))
