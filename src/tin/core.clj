(ns tin.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn tokenize [string]
  (str/split
   (str/replace (str/replace string "(" " ( ") ")" " ) ")
   #" "))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (tokenize "(+ 1 2)"))
  )
