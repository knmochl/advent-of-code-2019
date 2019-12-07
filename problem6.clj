(ns advent.problem6)

(defn add-orbit
  [orbits new-orbit]
  (assoc orbits (first new-orbit)
         (conj (get orbits (first new-orbit) []) (second new-orbit))))

(defn load-orbit-map
  [filename]
  (reduce add-orbit {} 
          (map #(clojure.string/split % #"\)")
               (clojure.string/split-lines (slurp filename)))))

(defn count-orbits
  [orbit-map orbits n accum]
  (if (empty? orbits)
    accum
    (let [next-orbits (mapcat #(get orbit-map %) orbits)]
      (recur orbit-map next-orbits (inc n) (+ accum (* n (count orbits)))))))

(defn problem6-1
  []
  (count-orbits (load-orbit-map "input6.txt") ["COM"] 0 0))
