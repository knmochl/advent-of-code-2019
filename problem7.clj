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

