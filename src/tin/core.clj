(ns tin.core
  (:require
   [clojure.string :as str]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]
   [clojure.core.match :refer [match]]
   [instaparse.core :as insta])
  (:gen-class))

(defn process-indentation
  "Returns a string which replaces indentation from the provided |reader| with
   indent and deindent tokens."
  [reader]
  (let [lines (line-seq reader)]))

(defn indentation-processor []
  (fn [xf]
    (let [previous-indent (volatile! ::none)]
      (fn
        [[] (xf)]
        ([result] (xf result))
        ([result line]
         (let [previous @previous-indent]
           (if (empty-line? line)
             result





(def parse (insta/parser (slurp "src/tin/grammar.ebnf")
                         :start :program
                         :auto-whitespace :standard))

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
   "=" (binary-operator "=")
   "log" (global-fn "console.log")
   })

(defn evaluate
  [value env]
  (match value
         [:symbol name] (get env name name)

         [:number val] val

         [:expression head & rest]
         (str/join " "
                   (apply (evaluate head env)
                          (map #(evaluate %1 env) rest)))

         [:program & expressions]
         (str/join "\n"
                   (map #(evaluate %1 env) expressions))))

(defn compile-input
  "Parses and evaluates the string in 'input' and returns the evaluated string
  output."
  [input]
  (evaluate (parse input) environment))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def input "(= abc 2)(log abc)")
  (def parsed (parse input))
  (println "parse>" parsed)
  (def evaluated (evaluate parsed environment))
  (println "js>" evaluated)
  (with-open [out (io/writer "output.js")]
    (.write out evaluated)
    (.write out "\n"))
  (let [{output :out} (shell/sh "node" "output.js")]
    (println "out>" output))
  )
