(ns advent.problem7
  [:require
   [advent.intcode :as intcode]
   [clojure.core.async :as async :refer [>!! <!! chan]]])

(defn run-amplifier
  [program-code phase input]
  (let [machine (intcode/make-machine program-code)]
    (do
      (intcode/execute-machine machine)
      (>!! (:input machine) phase)
      (>!! (:input machine) input)
      (<!! (:output machine)))))

(defn run-amps
  [program-code phase-list]
  (let [output1 (run-amplifier program-code (nth phase-list 0) 0)
        output2 (run-amplifier program-code (nth phase-list 1) output1)
        output3 (run-amplifier program-code (nth phase-list 2) output2)
        output4 (run-amplifier program-code (nth phase-list 3) output3)
        output5 (run-amplifier program-code (nth phase-list 4) output4)]
    output5))

(defn permutations
  [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn problem7-1
  []
  (let [program-code (intcode/load-program "input7.txt")]
    (apply max (map (partial run-amps program-code) (permutations [0 1 2 3 4])))))

(defn run-amps-with-feedback
  [phase-list]
  (let [program-code (intcode/load-program "input7.txt")
        machines (vec (repeatedly 5 (partial intcode/make-machine program-code)))
        final-output (async/mult (:output (nth machines 4)))
        feedback-chan (chan)
        output-chan (chan)]
    (do
      (doall (map #(async/pipe (:output (nth machines %)) (:input (nth machines (inc %)))) (range 0 4)))
      (async/tap final-output feedback-chan)
      (async/tap final-output output-chan)
      (async/pipe feedback-chan (:input (nth machines 0)))
      (doall (map intcode/execute-machine machines))
      (doall (map #(>!! (:input %2) %1) phase-list machines))
      (>!! (:input (nth machines 0)) 0)
    (loop [final-value (<!! output-chan)]
      (let [output (<!! output-chan)]
        (if (nil? output)
          final-value
          (recur output)))))))

(defn problem7-2
  []
  (apply max (map run-amps-with-feedback (permutations [5 6 7 8 9]))))
