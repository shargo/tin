(ns tin.lexer
  (:require
   [clojure.string :as str])
  (:gen-class))

(def token-regexes
  [
   ["SYMBOL" #"^[\p{L}][-+=!$%^&*<>_|\p{L}\p{N}]*"]
   ["KEYWORD" #"^[\p{L}]+:"]
   ["OPERATOR" #"^[-+=!$%^&*<>_][-+=!$%^&*<>_|\p{L}\p{N}]*"]
   ["NUMBER" #"^[\d]+"]
   ["DOT" #"^\."]
   ["COMMA" #"^,"]
   ["LPAREN" #"^\("]
   ["RPAREN" #"^\)"]
   ["WS" #"^[ ]+"]
   ["NEWLINE" #"^\n"]
   ])

(defn token-match
  "Returns a function which accepts a [token regex] pair and returns a [token
  remainder] pair if the regex matches 'string'."
  [string]
  (fn [[name regex]]
    (when-let [match (re-find regex string)]
      [name (subs string (count match))])))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  (some (token-match string) token-regexes))

(defn tokenize-string
  "Performs lexical analysis on string, returning a string containing the
   tokenized representation of the string or a LexicalError value."
  [string]
  (loop [rest string
         result []
         current-indentation 0
         is-continued-line? false
         line-number 0
         column-number 0]
    (if (empty? rest) result
        (let [[next-token remainder] (next-token rest)]
          (recur remainder
                 (conj result next-token)
                 current-indentation
                 is-continued-line?
                 line-number
                 column-number)))))
