(ns tin.lexer
  (:require
   [clojure.string :as str])
  (:gen-class))

(def ignored-regex #"(?:[ ]|/\*.*?\*/|//[^\n]*)*")

(defn token-regex
  [regex]
  (re-pattern (str #"(?s)^" "(" regex ")" ignored-regex)))

(def token-regexes
  [
   ["KEYWORD" #"[\p{L}]+:"]
   ["SYMBOL" #"[\p{L}][-+=!$%^&*<>_|\p{L}\p{N}]*"]
   ["OPERATOR" #"[-+=!$%^&*<>_][-+=!$%^&*<>_|\p{L}\p{N}]*"]
   ["NUMBER" #"[\d]+"]
   ["DOT" #"\."]
   ["COMMA" #","]
   ["LPAREN" #"\("]
   ["RPAREN" #"\)"]
   ["LINE_START" (re-pattern (str "(?:\n" ignored-regex ")*\n([ ])*"))]
   ])

(defn result-for-match
  [token-name match]
  (cond
    (some #{token-name} #{"SYMBOL" "KEYWORD" "OPERATOR" "NUMBER"})
    {:token (str token-name "(" (second match) ")")}

    (= "LINE_START" token-name)
    {:token "LINE_START" :indent (count (match 2))}

    :else
    {:token token-name}))

(defn token-match
  "Returns a function which accepts a [token regex] pair and returns a [token
  remainder] pair if the regex matches 'string'."
  [string]
  (fn [[name regex]]
    (when-let [match (re-find (token-regex regex) string)]
      [(result-for-match name match) (subs string (count (first match)))])))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  (some (token-match string) token-regexes))

(defn make-indent-token
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
