(ns advent.problem6
  [:require
   [clojure.java.io :as jio]])

(defn add-orbit
  [orbits new-orbit]
  (assoc orbits (first new-orbit)
         (conj (get orbits (first new-orbit) []) (second new-orbit))))

(defn load-orbit-map
  [filename]
  (reduce add-orbit {} 
          (map #(clojure.string/split % #"\)")
               (clojure.string/split-lines (slurp (jio/resource filename))))))

(defn count-orbits
  [orbit-map orbits n accum]
  (if (empty? orbits)
    accum
    (let [next-orbits (mapcat #(get orbit-map %) orbits)]
      (recur orbit-map next-orbits (inc n) (+ accum (* n (count orbits)))))))

(defn find-orbit
  [orbit-map body]
  (first (filter #(some #{body} (get orbit-map %)) (keys orbit-map))))

(defn path-to-center
  ([orbit-map body] (path-to-center orbit-map body []))
  ([orbit-map body path-list]
   (let [next-body (find-orbit orbit-map body)]
     (if (= next-body "COM")
       (conj path-list body)
       (recur orbit-map next-body (conj path-list body))))))

(defn count-transfers
  [orbit-map start dest]
  (let [start-path (drop 1 (path-to-center orbit-map start))
        dest-path (drop 1 (path-to-center orbit-map dest))
        dest-set (set dest-path)
        junction (first (drop-while (complement #(contains? dest-set %)) start-path))
        start->junction (take-while #(not= junction %) start-path)
        dest->junction (take-while #(not= junction %) dest-path)]
    (+ (count start->junction) (count dest->junction))))

(defn problem6-1
  []
  (count-orbits (load-orbit-map "input6.txt") ["COM"] 0 0))

(defn problem6-2
  []
  (count-transfers (load-orbit-map "input6.txt") "YOU" "SAN"))
