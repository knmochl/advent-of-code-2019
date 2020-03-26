(ns advent.problem13
  (:require [clojure.core.async :as async :refer [<!! >!! alts!! chan]]
            [lanterna.screen :as s]
            [advent.intcode :as intcode]))

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
  (let [x (<!! (:output machine))]
    (if (nil? x)
    tiles
    (let [y (<!! (:output machine))
          kind (<!! (:output machine))]
      (recur machine (process-tile tiles x y kind))))))

(defn draw-screen
  [screen tiles]
  (s/clear screen)
  (doseq [[x y :as key] (keys tiles)]
    (if (and (= x -1) (= y 0))
      (s/put-string screen 0 0 (str (get tiles key)))
      (s/put-string screen x (inc y) (get glyphs (get tiles key) "?"))))
  (s/redraw screen))

(defn problem13-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input13.txt"))
        run (intcode/execute-machine machine)
        screen (s/get-screen :swing)
        tiles (run-game machine {})]
    (do 
      (s/start screen)
      (draw-screen screen tiles)
      (s/get-key-blocking screen)
      (s/stop screen)
      (count (filter #(= 2 (second %)) tiles)))))

(defn play-game
  [machine tiles screen ball paddle]
  (let [machine-output (:output machine)
        machine-control (:control machine)
        [x channel] (alts!! [machine-output machine-control])]
      (if (= channel machine-control)
        (do
          (println "Send input")
          (>!! (:input machine) (compare (first ball) (first paddle)))
          (println (str (first ball) " " (first paddle) " " (compare (first ball) (first paddle))))
          (draw-screen screen tiles)
          (recur machine tiles screen ball paddle))
        (let [y (<!! machine-output)
              kind (<!! machine-output)
              new-tiles (process-tile tiles x y kind)]
          (case kind
            (0 1 2) (recur machine new-tiles screen ball paddle)
            3 (recur machine new-tiles screen ball [x y])
            4 (recur machine new-tiles screen [x y] paddle)
            nil (do (s/get-key-blocking screen)
                    (get new-tiles [-1 0]))
            (recur machine new-tiles screen ball paddle))))))

(defn problem13-2
  []
  (let [input-chan (chan 3)
        output-chan (chan 3)
        machine (intcode/make-machine (assoc (intcode/load-program "input13.txt") 0 2))
        screen (s/get-screen :swing)]
    (intcode/execute-machine machine)
    (s/in-screen screen (play-game machine {} screen nil nil))))
