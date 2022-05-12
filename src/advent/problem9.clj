(ns advent.problem9
  (:require [advent.intcode :as intcode]))

(defn problem9-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input9.txt"))
        machine (intcode/execute-machine (assoc machine :input 1))]
    (:output machine)))

(defn problem9-2
  []
  (let [machine (intcode/make-machine (intcode/load-program "input9.txt"))
        machine (intcode/execute-machine (assoc machine :input 2))]
    (:output machine)))
