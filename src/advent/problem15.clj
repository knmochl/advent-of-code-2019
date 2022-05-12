(ns advent.problem15
  (:require [advent.intcode :as intcode]))

(def left-direction [nil 3 4 2 1])

(def right-direction [nil 4 3 1 2])

(def direction-diffs [nil [0 1] [0 -1] [-1 0] [1 0]])

(defn move-robot
  [machine direction]
  (-> machine
      intcode/execute-machine
      (assoc :input direction)
      intcode/execute-machine))

(defn find-move
  [machine facing location]
  (loop [machine machine direction (nth left-direction facing)]
    (let [new-machine (move-robot machine direction)]
      (case (:output new-machine)
        0 (recur new-machine (nth right-direction direction))
        1 [new-machine direction (mapv + location (nth direction-diffs direction)) false]
        2 [new-machine direction (mapv + location (nth direction-diffs direction)) true]))))

(defn neighbor-distance
  [distances location]
  (let [neighbors (mapv
                   (partial mapv +)
                   (repeat location)
                   (rest direction-diffs))]
    (apply min (remove nil? (map #(get distances % nil) neighbors)))))

(defn find-generator
  [machine facing location distances]
  (let [[new-machine new-facing new-location done?] (find-move machine facing location)
        new-distance (inc
                      (neighbor-distance distances new-location))
        new-distances (if (contains? distances new-location)
                        distances
                        (assoc distances new-location new-distance))]
    (if done?
      [new-location new-distance]
      (recur new-machine new-facing new-location new-distances))))

(defn problem15-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input15.txt"))]
    (find-generator machine 1 [0 0] {[0 0] 0})))

(defn map-area
  [machine facing location distances]
  (let [[new-machine new-facing new-location done?] (find-move machine facing location)
        new-distance (inc
                      (neighbor-distance distances new-location))
        new-distances (if (contains? distances new-location)
                        distances
                        (assoc distances new-location new-distance))]
    (if (= new-location [0 0])
      (apply max (map second new-distances))
      (if (and done? (nil? (get distances new-location)))
        (recur new-machine new-facing new-location
               (into {} (map #(vector (first %)
                                      (Math/abs (- new-distance (second %))))
                             new-distances)))
        (recur new-machine new-facing new-location new-distances)))))

(defn problem15-2
  []
  (let [machine (intcode/make-machine (intcode/load-program "input15.txt"))]
    (map-area machine 1 [0 0] {[0 0] 0})))
