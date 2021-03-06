(ns advent.problem4
  [:require [clojure.edn :as edn]])

(defn number-to-list
  [number]
  (map (comp edn/read-string str first) (partition 1 (str number))))

(defn duplicate-digit?
  [numlist]
  (cond
    (< (count numlist) 2) false
    (= (first numlist) (second numlist)) true
    :else (recur (rest numlist))))

(defn ascending-digits?
  [numlist]
  (cond
    (< (count numlist) 2) true
    (> (first numlist) (second numlist)) false
    :else (recur (rest numlist))))

(defn pair-digit?
  [numlist]
  (pos? (count (filter #(= 2 %) (map count (partition-by identity numlist))))))

(defn valid-pass?
  [numlist]
  (and (duplicate-digit? numlist) (ascending-digits? numlist)))

(defn valid-pass2?
  [numlist]
  (and (duplicate-digit? numlist) (ascending-digits? numlist) (pair-digit? numlist)))

(defn problem4-1
  []
  (count (filter (comp valid-pass? number-to-list) (range 372304 847061))))

(defn problem4-2
  []
  (count (filter (comp valid-pass2? number-to-list) (range 372304 847061))))
