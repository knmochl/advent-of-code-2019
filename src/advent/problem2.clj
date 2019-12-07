(ns advent.problem2
  [:require
   [clojure.string :as str]
   [clojure.edn :as edn]])

(defn load-program
  [filename]
  (vec (map edn/read-string (str/split (str/trim-newline (slurp filename)) #","))))

(defn get-value
  [coll pos]
  (nth coll pos))

(defn set-value
  [coll pos new-value]
  (assoc coll pos new-value))

(defn run-opcode
  ([coll]
  (run-opcode coll 0))
  ([coll pos]
  (cond
    (= (get-value coll pos) 99) coll
    (= (get-value coll pos) 1)
    (let [value1 (get-value coll (get-value coll (+ pos 1)))
          value2 (get-value coll (get-value coll (+ pos 2)))
          loc3 (get-value coll (+ pos 3))
          new-coll (set-value coll loc3 (+ value1 value2))]
      (run-opcode new-coll (+ pos 4)))
    (= (get-value coll pos) 2)
    (let [value1 (get-value coll (get-value coll (+ pos 1)))
          value2 (get-value coll (get-value coll (+ pos 2)))
          loc3 (get-value coll (+ pos 3))
          new-coll (set-value coll loc3 (* value1 value2))]
      (run-opcode new-coll (+ pos 4)))
    :else "error")))

(defn problem2-1
  []
  (run-opcode (assoc (load-program "input2.txt") 1 12 2 2)))

(defn problem2-2
  []
  (let [initial (load-program "input2.txt")
        combos (mapcat #(map (partial vector %) (take 100 (range))) (take 100 (range)))
        results (map #(vector (first (run-opcode (assoc initial 1 (first %) 2 (second %)))) (first %) (second %)) combos)]
    (filter #(= (first %) 19690720) results)))

