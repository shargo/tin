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

(defn indent-size
  "Returns the count of whitespace characters at the beginning of |line|."
  [line]
  (count (take-while #(Character/isWhitespace %) (seq line))))

(defn indentation-processor
  "Transducer which adds indentation and end of line tokens to a line seq."
  [xf]
  (let [previous-indent (volatile! 0)]
    (fn
      ([] (xf))
      ([result]
       (if (> @previous-indent 0)
         ; Close any remaining indentation when we reach the end.
         (xf (xf result "«"))
         (xf result)))
       ([result line]
        (if (str/blank? line)
          result
          (let [token (cond (< @previous-indent (indent-size line)) "»≈"
                            (> @previous-indent (indent-size line)) "«≈"
                            :else "≈")]
            (vreset! previous-indent (indent-size line))
            (xf result (str token line))))))))

(defn add-indentation-tokens
  "Adds » and « tokens indicating indentation in the input |reader|."
  [reader]
   (str/join "\n" (into [] indentation-processor (line-seq reader))))

(def parse-string (insta/parser (slurp "src/tin/grammar.ebnf")
                                :start :program))

(defn parse
  "Parses the provided string into a parse tree"
  [string]
  (parse-string
   (add-indentation-tokens
    (io/reader (java.io.StringReader. string)))))

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
