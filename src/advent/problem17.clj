(ns advent.problem17
  (:require [advent.intcode :as intcode]))

(def test-input
  [
   [46 46 35 46 46 46 46 46 46 46 46 46 46]
   [46 46 35 46 46 46 46 46 46 46 46 46 46]
   [35 35 35 35 35 35 35 46 46 46 35 35 35]
   [35 46 35 46 46 46 35 46 46 46 35 46 35]
   [35 35 35 35 35 35 35 35 35 35 35 35 35]
   [46 46 35 46 46 46 35 46 46 46 35 46 46]
   [46 46 35 35 35 35 35 46 46 46 94 46 46]
   ])

(defn read-line-from-machine
  [machine]
  (loop [camera-machine machine row []]
    (if (= (:output camera-machine) 10)
      [(assoc camera-machine :output nil) row]
      (recur (intcode/execute-machine (assoc camera-machine :output nil)) (conj row (:output camera-machine))))))

(defn camera-view
  [machine]
  (loop [camera-machine (intcode/execute-machine machine) view []]
    (if (= (:status camera-machine) :halt)
      (drop-last view)
      (let [[new-machine new-row] (read-line-from-machine camera-machine)]
        (recur (intcode/execute-machine new-machine) (conj view new-row))))))

(defn get-location
  [viewport [x y]]
  (nth (nth viewport y) x))

(defn intersection?
  [viewport [x y]]
  (let [neighbors [[x y] [(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]
        neighbor-types (map (partial get-location viewport) neighbors)
        neighbor-scaffolds (map #(= 35 %) neighbor-types)]
    (every? true? neighbor-scaffolds)))

(defn find-intersections
  [viewport]
  (let [width (count (first viewport))
        height (count viewport)
        positions (for [x (range 1 (dec width)) y (range 1 (dec height))]
                    (vector x y))]
    (filter (partial intersection? viewport) positions)))

(defn part1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input17.txt"))
        viewport (camera-view machine)
        intersections (find-intersections viewport)]
    (apply + (map (partial apply *) intersections))))
