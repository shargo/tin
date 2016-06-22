(ns tin.core
  (:require [clojure.string :as str]
            [instaparse.core :as insta])
  (:gen-class))

(def parse (insta/parser (slurp "src/tin/grammar.ebnf")))


(defn evaluate
  [input]
  (case (first input)
    :number (read-string (second input))
    :symbol (second input)
    :expression (map evaluate (rest input))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def input "(+ 1 2)")
  (def parsed (parse input))
  (println parsed)
  ;(println (evaluate parsed))
  )
