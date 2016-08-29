(ns tin.common
  (:gen-class))

(defrecord Failure
    [type reason])

(defn failure [type reason] (Failure. type reason))

(defn failure? [value] (instance? Failure value))
