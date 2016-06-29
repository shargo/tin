(ns tin.core
  (:require
   [clojure.string :as str]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [instaparse.core :as insta])
  (:gen-class))

(def parse (insta/parser (slurp "src/tin/grammar.ebnf")))

(defn binary-operator [symbol]
  (fn [lhs rhs] (str lhs symbol rhs)))

(defn global-fn [name]
  (fn [& args] (str name "(" (str/join "," args) ")")))

(def environment
  {
   "+" (binary-operator "+")
   "-" (binary-operator "-")
   "*" (binary-operator "*")
   "/" (binary-operator "/")
   "log" (global-fn "console.log")
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
  (def input "(log (+ 1 (+ 2 3)))")
  (def parsed (parse input))
  (println parsed)
  (def evaluated (evaluate parsed environment))
  (println evaluated)
  (with-open [out (io/writer "output.js")]
    (.write out evaluated)
    (.write out "\n"))
  )

(-main)
