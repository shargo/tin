(ns tin.core
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta])
  (:gen-class))

(def parse (insta/parser (slurp "src/tin/grammar.ebnf")))

(defn binary-operator [symbol]
  (fn [lhs rhs] (str lhs symbol rhs)))

(def environment
  {
   "+" (binary-operator "+")
   "-" (binary-operator "-")
   "*" (binary-operator "*")
   "/" (binary-operator "/")
   })

(plusTwo 3)

(defn evaluate
  [value env]
  (match value
         [:symbol name] (env name)
         [:number val] val
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

(-main)
