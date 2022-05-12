(ns advent.problem13
  (:require [advent.intcode :as intcode]))

(def glyphs
  {0 "."
   1 "#"
   2 "X"
   3 "="
   4 "O"})

(defn remove-tile
  [tiles x y]
  (dissoc tiles [x y]))

(defn add-or-replace-tile
  [tiles x y kind]
  (assoc tiles [x y] kind))

(defn process-tile
  [tiles x y kind]
  (if (= kind 0)
    (remove-tile tiles x y)
    (add-or-replace-tile tiles x y kind)))

(defn run-game
  [machine tiles]
  (let [running-machine (intcode/execute-machine machine)]
    (if (= (:status running-machine) :halt)
      tiles
      (let [x (:output running-machine)
            running-machine (intcode/execute-machine running-machine)
            y (:output running-machine)
            running-machine (intcode/execute-machine running-machine)
            kind (:output running-machine)]
        (recur running-machine (process-tile tiles x y kind))))))

(defn problem13-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input13.txt"))
        tiles (run-game machine {})]
    (count (filter #(= 2 (second %)) tiles))))

(defn play-game
  [machine tiles ball paddle]
  (let [running-machine (intcode/execute-machine machine)
        status (:status running-machine)]
      (if (= status :input-needed)
        (let [direction (compare (first ball) (first paddle))
              running-machine (assoc running-machine :input direction)]
          (recur running-machine tiles ball paddle))
        (let [x (:output running-machine)
              running-machine (intcode/execute-machine running-machine)
              y (:output running-machine)
              running-machine (intcode/execute-machine running-machine)
              kind (:output running-machine)
              new-tiles (process-tile tiles x y kind)]
          (case kind
            (0 1 2) (recur running-machine new-tiles ball paddle)
            3 (recur running-machine new-tiles ball [x y])
            4 (recur running-machine new-tiles [x y] paddle)
            nil (get new-tiles [-1 0])
            (recur running-machine new-tiles ball paddle))))))

(defn problem13-2
  []
  (let [machine (intcode/make-machine (assoc (intcode/load-program "input13.txt") 0 2))]
  (play-game machine {} nil nil)))
