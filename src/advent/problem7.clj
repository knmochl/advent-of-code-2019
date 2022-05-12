(ns advent.problem7
  [:require
   [advent.intcode :as intcode]])

(defn run-amplifier
  [program-code phase input]
  (let [machine (intcode/make-machine program-code)
        machine (intcode/execute-machine (assoc machine :input phase))
        machine (intcode/execute-machine (assoc machine :input input))]
    (:output machine)))

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

(defn run-amp-cycle
  [[machine1 machine2 machine3 machine4 machine5]]
  (let [machine1 (intcode/execute-machine machine1)
        output1 (:output machine1)
        machine1 (intcode/execute-machine machine1)
        machine2 (intcode/execute-machine (assoc machine2 :input output1))
        output2 (:output machine2)
        machine2 (intcode/execute-machine machine2)
        machine3 (intcode/execute-machine (assoc machine3 :input output2))
        output3 (:output machine3)
        machine3 (intcode/execute-machine machine3)
        machine4 (intcode/execute-machine (assoc machine4 :input output3))
        output4 (:output machine4)
        machine4 (intcode/execute-machine machine4)
        machine5 (intcode/execute-machine (assoc machine5 :input output4))
        output5 (:output machine5)
        machine5 (intcode/execute-machine machine5)]
    [(assoc machine1 :input output5) machine2 machine3 machine4 machine5]))

(defn run-amps-with-feedback
  [phase-list]
  (let [program-code (intcode/load-program "input7.txt")
        machines (vec (repeatedly 5 (partial intcode/make-machine program-code)))
        machines (map #(assoc %1 :input %2) machines phase-list)
        primed-machines (mapv intcode/execute-machine machines)
        primed-machines (assoc-in primed-machines [0 :input] 0)]
    (loop [machines (run-amp-cycle primed-machines)]
      (if (= (:status (nth machines 4)) :halt)
        (:output (nth machines 4))
        (recur (run-amp-cycle machines))))))

(defn problem7-2
  []
  (apply max (map run-amps-with-feedback (permutations [5 6 7 8 9]))))
