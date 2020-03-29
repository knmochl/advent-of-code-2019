(ns advent.problem16
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]))

(defn fft-sequence
  [n]
  (drop 1 (cycle (apply concat (map (partial repeat n) [0 1 0 -1])))))

(defn fft-process-digit
  [input n]
  (mod (Math/abs (reduce + (map * input (fft-sequence (inc n))))) 10))

(defn fft-process
  [input]
  (map (partial fft-process-digit input) (range (count input))))

(defn load-input
  [filename]
  (vec (drop-last (map (comp edn/read-string str) (slurp (jio/resource filename))))))

(defn problem16-1
  []
  (let [start (load-input "input16.txt")]
    (take 8 (last (take 101 (iterate fft-process start))))))
