(ns tin.core
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta])
  (:gen-class))

(def parse (insta/parser (slurp "src/tin/grammar.ebnf")))

(def environment
  {
   "+" +
  })

(defn evaluate
  [value env]
  (match value
    [:symbol name] (env name)
    [:number val] (read-string val)
    [:expression head & rest] (apply (evaluate head env)
                                     (map #(evaluate %1 env) rest))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def input "(+ 1 (+ 2 3))")
  (def parsed (parse input))
  (println parsed)
  (println (evaluate parsed environment))
  ;(println (evaluate parsed))
  )
