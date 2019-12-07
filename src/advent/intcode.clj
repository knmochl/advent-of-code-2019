(ns advent.intcode
  [:require
   [clojure.java.io :as jio]
   [clojure.string :as str]])

(defn get-value
  [memory pos mode]
  (let [mem-val (nth memory pos)]
    (if (= mode 0)
      (get-value memory mem-val 1)
      mem-val)))

(defn set-value
  [memory pos new-value]
  (assoc memory pos new-value))

(defn decode-modes
  ([mode] (decode-modes mode []))
  ([mode accum]
   (if (pos? mode)
     (recur (quot mode 10) (conj accum (rem mode 10)))
     accum)))

(defn decode-opcode
  [instruction]
  (let [opcode (rem instruction 100)
        modes (decode-modes (quot instruction 100))]
    [opcode modes]))

(defn get-mode
  [modes n]
  (get modes (dec n) 0))

(defn execute-opcode
  [memory instruction-pointer input output]
  (let [[opcode modes] (decode-opcode (get-value memory instruction-pointer 1))]
    (cond
      (= opcode 99) nil
      (= opcode 1)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value memory (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value memory (+ instruction-pointer 3) 1)
            new-memory (set-value memory target (+ value1 value2))]
        [new-memory (+ instruction-pointer 4) input output])
      (= opcode 2)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value memory (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value memory (+ instruction-pointer 3) 1)
            new-memory (set-value memory target (* value1 value2))]
        [new-memory (+ instruction-pointer 4) input output])
      (= opcode 3)
      (let [target (get-value memory (+ instruction-pointer 1) 1)
            new-memory (set-value memory target (first input))
            new-input (rest input)]
        [new-memory (+ instruction-pointer 2) new-input output])
      (= opcode 4)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            new-output (conj output value1)]
        [memory (+ instruction-pointer 2) input new-output])
      (= opcode 5)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            new-ip (get-value memory (+ instruction-pointer 2) (get-mode modes 2))]
        (if (= value1 0)
          [memory (+ instruction-pointer 3) input output]
          [memory new-ip input output]))
      (= opcode 6)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            new-ip (get-value memory (+ instruction-pointer 2) (get-mode modes 2))]
        (if (= value1 0)
          [memory new-ip input output]
          [memory (+ instruction-pointer 3) input output]))
      (= opcode 7)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value memory (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value memory (+ instruction-pointer 3) 1)
            new-memory (if (< value1 value2)
                         (set-value memory target 1)
                         (set-value memory target 0))]
        [new-memory (+ instruction-pointer 4) input output])
      (= opcode 8)
      (let [value1 (get-value memory (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value memory (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value memory (+ instruction-pointer 3) 1)
            new-memory (if (= value1 value2)
                         (set-value memory target 1)
                         (set-value memory target 0))]
        [new-memory (+ instruction-pointer 4) input output])
      :else "error")))

(defn run-opcode
  ([memory]
   (run-opcode memory 0 [] []))
  ([memory instruction-pointer input output]
   (let [result (execute-opcode memory instruction-pointer input output)]
     (cond
       (= result nil) [memory instruction-pointer input output]
       (= result "error") "error"
       :else (let [[new-mem new-ip new-input new-output] result]
               (recur new-mem new-ip new-input new-output))))))

(defn load-program
  [filename]
  (vec (map #(Integer. %) (str/split (str/trim-newline (slurp (jio/resource filename))) #","))))
