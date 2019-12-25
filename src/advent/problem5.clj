(ns advent.problem5
  [:require
   [advent.intcode :as intcode]
   [clojure.core.async :as async :refer [>!! <!!]]])

(defn problem5-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input5.txt"))]
    (do (intcode/execute-machine machine)
        (>!! (:input machine) 1)
        (<!! (async/into [] (:output machine))))))

(defn problem5-2
  []
  (let [machine (intcode/make-machine (intcode/load-program "input5.txt"))]
    (do (intcode/execute-machine machine)
        (>!! (:input machine) 5)
        (<!! (async/into [] (:output machine))))))
