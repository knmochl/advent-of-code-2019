(ns advent.problem5
  [:require
   [advent.intcode :as intcode]])

(defn init-diagnostic
  [machine identifier]
  (intcode/execute-machine (assoc machine :input identifier)))

(defn run-diagnostic
  [machine output]
  (if (= (:status machine) :halt)
    output
    (recur (intcode/execute-machine machine) (conj output (:output machine)))))

(defn problem5-1
  []
  (let [machine (intcode/make-machine (intcode/load-program "input5.txt"))
        machine (init-diagnostic machine 1)
        diags (run-diagnostic machine [])]
    (last diags)))

(defn problem5-2
  []
  (let [machine (intcode/make-machine (intcode/load-program "input5.txt"))
        machine (init-diagnostic machine 5)
        diags (run-diagnostic machine [])]
    (last diags)))
