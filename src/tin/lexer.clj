(ns tin.lexer
  (:require
   [clojure.string :as str])
  (:gen-class))

(def ignored-regex #"(?s)(?:[ ]|/\*.*?\*/)*")

(defn token-regex
  [regex]
  (re-pattern (str #"^" regex ignored-regex)))

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
   ["LINE_START" #"\n"]
   ])

(defn result-for-match
  [token-name match]
  (if (some #{token-name} #{"SYMBOL" "KEYWORD" "OPERATOR" "NUMBER"})
    (str token-name "(" match ")")
    token-name))

(defn token-match
  "Returns a function which accepts a [token regex] pair and returns a [token
  remainder] pair if the regex matches 'string'."
  [string]
  (fn [[name regex]]
    (when-let [match (re-find (token-regex regex) string)]
      [(result-for-match name match) (subs string (count match))])))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  (some (token-match string) token-regexes))

(defn tokenize-string
  "Performs lexical analysis on string, returning a string containing the
   tokenized representation of the string or a LexicalError value."
  [string]
  (loop [rest (str "\n" string "\n")
         result []
         current-indentation 0
         logical-line? false
         line-number 0
         column-number 0]
    (if (empty? rest) result
        (let [[next-token remainder] (next-token rest)]
          (recur remainder
                 (conj result next-token)
                 current-indentation
                 logical-line?
                 line-number
                 column-number)))))
