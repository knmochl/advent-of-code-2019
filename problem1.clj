(ns advent.problem1
  (:require
   [clojure.string]
   [clojure.edn]))

(defn fuel-required
  [mass]
  (- (quot mass 3) 2))

(defn parse-input
  [filename]
  (map clojure.edn/read-string (clojure.string/split-lines (slurp filename))))

(defn count-fuel
  [masses]
  (reduce + 0 (map fuel-required masses)))

(defn problem1
  []
  (count-fuel (parse-input "prob1-1.input.txt")))

