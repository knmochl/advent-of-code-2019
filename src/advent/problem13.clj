(ns advent.problem13
  (:require [clojure.core.async :as async :refer [<!! >!!]]
            [advent.intcode :as intcode]))

(defn remove-tile
  [tiles x y]
  (dissoc tiles [x y]))

(defn add-or-replace-tile
  [tiles x y kind]
  (assoc tiles [x y] kind))

(defn process-tile
  [tiles x y kind]
  (case kind
    0 (remove-tile tiles x y)
    (1 2 3 4) (add-or-replace-tile tiles x y kind)))

(defn run-game
  [machine tiles]
  (let [x (<!! (:output machine))]
    (if (nil? x)
    tiles
    (let [y (<!! (:output machine))
          kind (<!! (:output machine))]
      (recur machine (process-tile tiles x y kind))))))

(defn problem13-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input13.txt"))
        run (intcode/execute-machine machine)
        tiles (run-game machine {})]
    (count (filter #(= 2 (second %)) tiles))))
