(ns advent.problem3)

(defn wire-positions
  [[start-x start-y] wire]
  (let [direction (first wire)
        distance (-> wire rest clojure.string/join Integer.)]
    (cond
      (= direction \R)
      (for [x (range (inc start-x) (+ start-x (inc distance)))] [x start-y])
      (= direction \L)
      (for [x (range (dec start-x) (- start-x (inc distance)) -1)] [x start-y])
      (= direction \U)
      (for [y (range (inc start-y) (+ start-y (inc distance)))] [start-x y])
      (= direction \D)
      (for [y (range (dec start-y) (- start-y (inc distance)) -1)] [start-x y]))))

(defn run-wires
  ([wires] (run-wires wires [[0 0]]))
  ([wires positions]
   (if (empty? wires)
     positions
     (recur (rest wires) (concat positions (wire-positions (last positions) (first wires)))))))

(defn load-wires
  [filename]
  (map #(clojure.string/split % #",") (-> filename slurp clojure.string/split-lines)))

(defn find-intersections
  [wire-list]
  (apply clojure.set/intersection (map #(set (drop 1 (run-wires %))) wire-list)))

(defn wire-delay
  [wires intersection]
  (apply + (map #(.indexOf % intersection) wires)))

(defn problem3
  []
  (first (sort (map #(+ (Math/abs (first %)) (Math/abs (second %))) (find-intersections (load-wires "input3.txt"))))))

(defn problem3-2
  []
  (let [wires (load-wires "input3.txt")
        wire-positions (map run-wires wires)
        intersections (find-intersections wires)]
    (first (sort (map #(wire-delay wire-positions %) intersections)))))

