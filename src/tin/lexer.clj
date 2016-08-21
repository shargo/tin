(ns tin.lexer
  (:require
   [clojure.string :as str])
  (:gen-class))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  [])

(defn tokenize-string
  "Performs lexical analysis on string, returning a string containing the
   tokenized representation of the string or a LexicalError value."
  [string]
  (loop [rest string
         result []
         current-indentation 0
         is-continued-line? false]
    (empty? rest) result
    (let [[next-token remainder] (next-token rest)]
      (recur remainder
             (conj result next-token)
             current-indentation
             is-continued-line?))))
