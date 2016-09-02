(ns tin.parser
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]
   [tin.lexer :as lexer]
   [tin.common :as common])
  (:gen-class))

(def parse-string (insta/parser (slurp "src/tin/grammar.ebnf")
                                :start :program
                                :auto-whitespace :standard))

(defn parse
  "Parses the provided string into a parse tree"
  [string]
  (let [tokens (lexer/tokenize-string string)]
    (if (common/failure? tokens)
      tokens
      (let [tokenized (str/join "\n" tokens)
            trees (insta/parses parse-string tokenized)]
        (cond
          (> (count trees) 1)
          (throw (RuntimeException. (str "Ambiguous parse '" string "'")))
          (= 0 (count trees))
          (parse-string tokenized)
          :else
          (first trees))))))
