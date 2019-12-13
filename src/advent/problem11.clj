(ns advent.problem11
  (:require [advent.intcode :as intcode]))

(defn initialize-robot
  [filename start-color]
  {:computer (intcode/make-machine (intcode/load-program filename) [start-color])
   :direction [0 1]
   :location [0 0]
   :tiles #{}
   :all-tiles #{}
   :complete false})

(defn change-direction
  [current turn]
  (if (= turn 0)
    (condp = current
      [0 1] [-1 0]
      [-1 0] [0 -1]
      [0 -1] [1 0]
      [1 0] [0 1])
    (condp = current
      [0 1] [1 0]
      [1 0] [0 -1]
      [0 -1] [-1 0]
      [-1 0] [0 1])))

(defn move-robot
  [robot]
  (let [paint-computer (intcode/run-to-output (:computer robot))]
    (if (nil? (:ip paint-computer))
      (assoc robot :complete true)
      (let [should-paint (intcode/get-output paint-computer)
            turn-computer (intcode/run-to-output (assoc paint-computer :output []))
            turn-direction (intcode/get-output turn-computer)
            new-direction (change-direction (:direction robot) turn-direction)
            new-location (vec (map #(+ (get-in robot [:location %])
                                       (get new-direction %)) [0 1]))
            new-tiles (if (= should-paint 0)
                        (disj (robot :tiles) (robot :location))
                        (conj (robot :tiles) (robot :location)))
            new-all-tiles (conj (robot :all-tiles) (robot :location))
            tile-painted (if (contains? (robot :tiles) new-location) 1 0)
            new-computer (assoc turn-computer :input [tile-painted] :output [])]
        (assoc robot :computer new-computer
               :direction new-direction
               :location new-location
               :tiles new-tiles
               :all-tiles new-all-tiles)))))

(defn run-robot
  [robot]
  (let [new-robot (move-robot robot)]
    (if (:complete new-robot)
      new-robot
      (recur new-robot))))

(defn problem11-1
  []
  (let [robot (run-robot (initialize-robot "input11.txt" 0))]
    (count (:all-tiles robot))))

(defn make-output-line
  [tiles min-x max-x]
  (let [positions (set (map first tiles))]
    (clojure.string/join (map #(if (contains? positions %) "X" " ") (range min-x (inc max-x))))))

(defn problem11-2
  []
  (let [robot (run-robot (initialize-robot "input11.txt" 1))
        tiles (:tiles robot)
        min-x (apply min (map first tiles))
        max-x (apply max (map first tiles))
        min-y (apply min (map second tiles))
        max-y (apply max (map second tiles))
        grouped-tiles (group-by second tiles)]
    (map #(make-output-line (grouped-tiles %) min-x max-x) (reverse (sort (keys grouped-tiles))))))
