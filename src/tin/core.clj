(ns tin.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn tokenize [string]
  (filter #(not= %1 "")
          (str/split
           (str/replace
            (str/replace string "(" " ( ") ")" " ) ")
           #" ")))

(defn atomic [token] {:token token})

(defn read-from-tokens [tokens]
  ;(println "read-from-tokens>" tokens "<")
  (if (zero? (count tokens))
    (throw (Exception. "Unexpected EOF while reading")))
  (let [[token & tokens] tokens]
    (case token
      "(" (into [] (map read-from-tokens (take-while #(not= ")" %1) tokens)))
      ")" (throw (Exception. "Unexpected ) while reading"))
      (atomic token))))

(defn parse [string]
  (read-from-tokens (tokenize string)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (str/join "," (tokenize "(+ 1 2)")))
  (println (parse "(+ 1 2)"))
  )
