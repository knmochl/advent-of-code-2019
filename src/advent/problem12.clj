(ns advent.problem12
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn process-moon
  [line]
  (vec (map edn/read-string (drop 1 (re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>" line)))))

(defn load-scan
  [filename]
  (into {} (map #(vector %1 {:vel [0 0 0] :pos (process-moon %2)}) '(:io :ganymede :callisto :europa) (str/split-lines (slurp (jio/resource filename))))))

(defn gravity-velocity-axis
  [ours theirs]
  (cond
    (> theirs ours) 1
    (< theirs ours) -1
    (= theirs ours) 0))

(defn gravity-velocity
  [ours theirs]
  (vec (map #(gravity-velocity-axis (get ours %) (get theirs %)) (range 0 3))))

(defn add-velocity
  [current delta]
  (vec (map #(+ (get current %) (get delta %)) (range 0 3))))

(defn update-velocities
  [moons]
  (reduce-kv (fn [m k v]
               (assoc-in m [k :vel]
                         (reduce add-velocity (get-in m [k :vel])
                                 (map #(gravity-velocity (get-in m [k :pos])
                                                         (get-in m [% :pos]))
                                      (remove (partial = k) (keys m)))))) moons moons))

(defn update-positions
  [moons]
  (reduce-kv (fn [m k v]
               (assoc-in m [k :pos]
                         (vec (map #(+ ((v :pos) %) ((v :vel) %)) (range 0 3)))))
             moons moons))

(def update-moons (comp update-positions update-velocities))

(defn moon-energy
  [moon]
  (*
   (apply + (map #(Math/abs %) (:vel moon)))
   (apply + (map #(Math/abs %) (:pos moon)))))

(defn problem12-1
  []
  (apply +
         (map #(moon-energy (second %))
              (nth (iterate update-moons
                            (load-scan "input12.txt")) 1000))))

(defn find-duplicate
  [moons previous-states counter]
  (do (when (= 0 (rem counter 100000)) (println counter))
    (if (contains? previous-states [(hash moons) (.hashCode moons)])
     [counter moons]
     (recur (update-moons moons) (conj previous-states [(hash moons ) (.hashCode moons)]) (inc counter)))))

(defn moons-by-axis
  [moons n]
  (mapcat #(vector (nth (:pos (second %)) n) (nth (:vel (second %)) n)) moons))

(defn find-periods
  [moons initial-state period-list counter]
  (if (every? (complement nil?) period-list)
    period-list
    (let [new-moons (update-moons moons)
          new-axis (map (partial moons-by-axis new-moons) (range 0 3))
          new-counter (inc counter)
          new-periods (mapv #(if (= %1 %2) (or %3 new-counter) %3) initial-state new-axis period-list)]
      (do (when (= 0 (rem counter 100000)) (println (str counter " " new-periods))) new-moons (recur new-moons initial-state new-periods (inc counter))))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn problem12-2
  []
  (let [moons (load-scan "input12.txt")
        initial-state (map (partial moons-by-axis moons) (range 0 3))
        periods (find-periods moons initial-state [nil nil nil] 0)]
    (reduce lcm periods)))
