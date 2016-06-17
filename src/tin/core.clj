(ns tin.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn tokenize [string]
  (str/split
   (str/replace (str/replace string "(" " ( ") ")" " ) ")
   #" "))

(defn parse [string]
  (read-from-tokens (tokenize string)))

(defn read-from-tokens [tokens]
  (if (zero? (count tokens))
    (throw (Exception. "Unexpected EOF while reading")))
  (let [[token & tokens] tokens]
    (case token
      "(" (map read-from-tokens (take-while #(not= ")" %1) tokens))
      ")" (throw (Exception. "Unexpected ) while reading"))
      (atom token))))

(defn atom)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (tokenize "(+ 1 2)"))
  )
