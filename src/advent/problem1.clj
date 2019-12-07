(ns advent.problem1
  (:require
   [[clojure.string]
    [clojure.edn]
    [clojure.java.io :as jio]]))

(defn fuel-required
  [mass]
  (- (quot mass 3) 2))

(defn module-fuel-required
  [module-mass]
  (reduce + 0 (take-while pos? (iterate fuel-required (fuel-required module-mass)))))

(defn parse-input
  [filename]
  (map clojure.edn/read-string (clojure.string/split-lines (slurp filename))))

(defn count-fuel
  [modules]
  (reduce + 0 (map module-fuel-required modules)))

(defn problem1
  []
  (count-fuel (parse-input (jio/resource "input1.txt"))))

