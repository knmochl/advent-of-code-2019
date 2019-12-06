(ns advent.problem5
  [:require
   [advent.intcode :as intcode]])

(defn problem5-1
  []
  (nth (advent.intcode/run-opcode (advent.intcode/load-program "input5.txt") 0 [1] []) 3))

