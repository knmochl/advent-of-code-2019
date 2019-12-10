(ns advent.problem9
  (:require [advent.intcode :as intcode]))

(defn problem9-1
  []
  (:output (intcode/run-opcode (intcode/make-machine (intcode/load-program "input9.txt") [1]))))

(defn problem9-2
  []
  (:output (intcode/run-opcode (intcode/make-machine (intcode/load-program "input9.txt") [2]))))
