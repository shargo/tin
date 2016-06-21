(ns tin.core
  (:require [clojure.string :as str]
            [instaparse.core :as insta])
  (:gen-class))

(def parse
  (insta/parser
   "number = #'[0-9]+'"))

(defn evaluate
  [[symbol value]]
  (case symbol
    :number (read-string value)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (def input "12")
  (def parsed (parse input))
  (println parsed)
  (println (evaluate parsed))
  )
