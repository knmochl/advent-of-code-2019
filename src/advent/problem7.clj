(ns advent.problem7
  [:require
   [advent.intcode :as intcode]])

(defn run-amplifier
  [program-code phase input]
  (first (:output (intcode/run-opcode (intcode/make-machine program-code [phase input])))))

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
  (loop [machines (assoc-in (vec (map #(intcode/make-machine (intcode/load-program "input7.txt") [%]) phase-list)) [0 :input 1] 0)
         current-amp 0
         current-output 0]
    (let [new-machine (intcode/run-to-output (nth machines current-amp))]
      (if (nil? (:ip new-machine))
        current-output
        (let [next-amp (if (= current-amp 4) 0 (inc current-amp))
              next-output (first (:output new-machine))
              cleared-output (assoc-in machines [current-amp :output] [])
              added-input (assoc-in cleared-output [next-amp :input] [next-output])]
          (recur added-input next-amp next-output))))))

(defn problem7-2
  []
  (apply max (map run-amps-feedback (permutations [5 6 7 8 9]))))
