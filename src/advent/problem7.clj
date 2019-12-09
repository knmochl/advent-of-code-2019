(ns advent.problem7
  [:require
   [advent.intcode :as intcode]])

(defn run-amplifier
  [program-code phase input]
  (first (last (intcode/run-opcode program-code 0 [phase input] []))))

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

(defn run-feedback-amp
  [memory instruction-pointer input output]
  (let [result (intcode/execute-opcode memory instruction-pointer input output)]
    (cond
      (= result nil) nil
      (= result "error") "error"
      (not (empty? (nth result 3))) result
      :else (let [[new-mem new-ip new-input new-output] result]
              (recur new-mem new-ip new-input new-output)))))

(defn run-amps-feedback
  [phase-list]
  (loop [memories (vec (repeat 5 (intcode/load-program "input7.txt")))
         ips [0 0 0 0 0]
         inputs (assoc-in (vec (map vector phase-list)) [0 1] 0)
         outputs [[] [] [] [] []]
         current-amp 0
         current-output 0]
    (let [result (run-feedback-amp (nth memories current-amp)
                                   (nth ips current-amp)
                                   (nth inputs current-amp)
                                   (nth outputs current-amp))]
      (if (vector? result)
        (let [next-amp (if (= current-amp 4) 0 (inc current-amp))
              next-output (first (get result 3))
              new-memories (assoc memories current-amp (nth result 0))
              new-ips (assoc ips current-amp (nth result 1))
              new-inputs (assoc (assoc inputs current-amp (nth result 2)) next-amp (conj (get inputs next-amp) next-output))]
          (recur new-memories new-ips new-inputs outputs next-amp next-output))
        current-output))))

(defn problem7-2
  []
  (apply max (map run-amps-feedback (permutations [5 6 7 8 9]))))
