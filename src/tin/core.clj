(ns tin.core
  (:require [clojure.string :as str]
            [instaparse.core :as insta])
  (:gen-class))

(def parse
  (insta/parser
   "number = #'[0-9]+'"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (def input "12")
  (println (parse input))
  )
