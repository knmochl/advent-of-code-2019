(ns advent.problem5
  [:require
   [advent.intcode :as intcode]])

(defn problem5-1
  []
  (:output (advent.intcode/run-opcode (advent.intcode (advent.intcode/load-program "input5.txt") [1]))))

(defn problem5-2
  []
  (:output (advent.intcode/run-opcode (advent.intcode (advent.intcode/load-program "input5.txt") [5]))))

