(ns advent.problem10
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]))

(defn process-line
  [line y]
  (keep #(if (= (get line %) \#) [y %] nil) (range 0 (count line))))

(defn load-asteroids
  [filename]
  (let [lines (str/split-lines (slurp (jio/resource filename)))
        y-range (range 0 (count lines))
        x-size (count (first lines))
        y-size (count lines)]
    [(set (mapcat #(process-line (get lines %) %) y-range)) y-size x-size]))

(defn possible-obstructions
  [start end]
  (if (= (first start) (first end))
    (map #(vector (first start) %) (range (inc (min (second start) (second end))) (max (second start) (second end))))
    (let [y-start (first start)
          x-start (second start)
          y-end (first end)
          x-end (second end)
          y-dist (- y-start y-end)
          x-dist (- x-start x-end)
          slope (/ x-dist y-dist)
          calc-x (fn [y] (+ x-start (* (- y y-start) slope)))]
     (map (fn [y] (vector y (calc-x y))) (range (inc (min y-start y-end)) (max y-start y-end))))))

(defn obstructed?
  [asteroid-map start end]
  (some #(contains? asteroid-map %) (possible-obstructions start end)))

(defn count-visible
  [asteroid-map start]
  (count (filter (complement (partial obstructed? asteroid-map start)) (disj asteroid-map start))))

(defn problem10-1
  []
  (let [asteroid-map (first (load-asteroids "input10.txt"))]
    (apply max (map (partial count-visible asteroid-map) asteroid-map))))

(defn manhattan-distance
  [start end]
  (+ (Math/abs (- (first start) (first end)))
     (Math/abs (- (second start) (second end)))))

(defn nearest-asteroid
  [asteroid-map start end]
  (first (sort-by (partial manhattan-distance start)
                  (filter #(contains? asteroid-map %)
                          (possible-obstructions start end)))))

(defn next-end
  [end x-edge y-edge]
  (let [y (first end)
        x (second end)]
    (cond
      (and (= y -1) (not= x x-edge))
      [y (inc x)]
      (and (= x x-edge) (not= y y-edge))
      [(inc y) x]
      (and (= y y-edge) (not= x -1))
      [y (dec x)]
      (and (= x -1) (not= y -1))
      [(dec y) x])))

(defn shoot-asteroids
  ([angle-map origin]
   (shoot-asteroids angle-map origin []))
  ([angle-map origin accum]
   (if (empty? angle-map)
     (vec accum)
     (let [shot-list (map #(first (angle-map %)) (sort (keys angle-map)))
           new-angles (into {} (map (fn [[key value]] (if (empty? (rest value)) nil [key (rest value)])) angle-map))]
       (recur new-angles origin (concat accum shot-list))))))

(defn angle
  [start end]
  (let [initial-angle (Math/toDegrees (Math/atan2 (- (second end) (second start)) (- (first start) (first end))))]
    (if (< initial-angle 0)
      (+ 360 initial-angle)
      initial-angle)))

(defn problem10-2
  []
  (let [asteroid-map (first (load-asteroids "input10.txt"))
        visible-counts (map #(vector % (count-visible asteroid-map %)) asteroid-map)
        observe-point (first (last (sort-by second visible-counts)))
        angle-map (group-by (partial angle observe-point) (disj asteroid-map observe-point))
        sorted-angles (into {} (map (fn [[key value]] [key (sort-by (partial manhattan-distance observe-point) value)]) angle-map))
        asteroids (shoot-asteroids sorted-angles observe-point)
        target (get asteroids 199)]
    (+ (* 100 (second target)) (first target))))
