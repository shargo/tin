(ns tin.lexer
  (:require
   [clojure.string :as str])
  (:gen-class))

(defn token-regex
  [regex]
  (re-pattern (str #"(?s)^" "(" regex ")")))

(def token-regexes
  [
   ["KEYWORD" (token-regex #"[\p{L}]+:")]
   ["SYMBOL" (token-regex #"[\p{L}][-+=!$%^&*<>_|\p{L}\p{N}]*")]
   ["OPERATOR" (token-regex #"[-+=!$%^&*<>_][-+=!$%^&*<>_|\p{L}\p{N}]*")]
   ["NUMBER" (token-regex #"[\d]+")]
   ["DOT" (token-regex #"\.")]
   ["COMMA" (token-regex #",")]
   ["LPAREN" (token-regex #"\(")]
   ["RPAREN" (token-regex #"\)")]
   ["LINE_START" #"^(?s)(?:\n(?:[ ]|/\*.*?\*/|//[^\n]*)*)*\n([ ])*"]
   ["WS" #"^(?s)([ ]|/\*.*?\*/|//[^\n]*)+"]
   ])

(defn result-for-match
  "Returns a token object for the provided regex match object."
  [token-name match]
  (cond
    (some #{token-name} #{"SYMBOL" "KEYWORD" "OPERATOR" "NUMBER"})
    {:token (str token-name "(" (second match) ")")}

    (= "LINE_START" token-name)
    {:token "LINE_START" :indent (count (second match))}

    :else
    {:token token-name}))

(defn token-match
  "Returns a function which accepts a [token regex] pair and returns a [token
  remainder] pair if the regex matches 'string'."
  [string]
  (fn [[name regex]]
    (when-let [match (re-find regex string)]
      [(result-for-match name match) (subs string (count (first match)))])))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  (some (token-match string) token-regexes))

(defn make-indent-token
  "Returns a pair consisting of an indentation token to insert (or nil) and the
  new current indentation based on the provided line indentation and current
  indentation"
  [line-indent current-indent]
  (cond
    (or (nil? line-indent) (= line-indent current-indent))
    [nil current-indent]
    (< line-indent current-indent)
    ["DEDENT" line-indent]
    (> line-indent current-indent)
    ["INDENT" line-indent]))

(defn tokenize-string
  "Performs lexical analysis on string, returning a string containing the
   tokenized representation of the string or a LexicalError value."
  [string]
  (loop [rest (str "\n" string "\n")
         result []
         current-indent 0
         is-line-continuation? false
         line-number 0
         column-number 0]
    (if (empty? rest) result
        (let [[{token :token indent :indent} remainder]
              (next-token rest)
              [indent-token new-indent]
              (make-indent-token indent current-indent)]
          (recur remainder
                 (if indent-token
                   (conj result indent-token token)
                   (conj result token))
                 new-indent
                 is-line-continuation?
                 line-number
                 column-number)))))
