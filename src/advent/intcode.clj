(ns advent.intcode
  [:require
   [clojure.java.io :as jio]
   [clojure.string :as str]
   [clojure.edn :as edn]])

(defn get-value
  [machine pos mode]
  (let [memory (:memory machine)
        mem-val (get memory pos 0)]
    (cond
      (= mode 0)
      (get-value machine mem-val 1)
      (= mode 1)
      mem-val
      (= mode 2)
      (get-value machine (+ (:relative machine) mem-val) 1))))

(defn set-value
  [memory pos new-value]
  (let [mem-size (count memory)]
    (if (> pos mem-size)
      (conj (into memory (repeat (- pos mem-size) 0)) new-value)
      (assoc memory pos new-value))))

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
  [machine]
  (let [memory (:memory machine)
        instruction-pointer (:ip machine)
        [opcode modes] (decode-opcode (get-value machine instruction-pointer 1))]
    (cond
      (= opcode 99)
      (assoc machine :ip nil)
      (= opcode 1)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value machine (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value machine (+ instruction-pointer 3) 1)
            new-memory (set-value memory target (+ value1 value2))
            new-ip (+ instruction-pointer 4)]
        (assoc machine :memory new-memory :ip new-ip))
      (= opcode 2)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value machine (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value machine (+ instruction-pointer 3) 1)
            new-memory (set-value memory target (* value1 value2))
            new-ip (+ instruction-pointer 4)]
        (assoc machine :memory new-memory :ip new-ip))
      (= opcode 3)
      (let [target (get-value machine (+ instruction-pointer 1) 1)
            input (:input machine)
            new-memory (set-value memory target (first input))
            new-input (rest input)
            new-ip (+ instruction-pointer 2)]
        (assoc machine :memory new-memory :ip new-ip :input new-input))
      (= opcode 4)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            output (:output machine)
            new-output (conj output value1)
            new-ip (+ instruction-pointer 2)]
        (assoc machine :ip new-ip :output new-output))
      (= opcode 5)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            new-ip (get-value machine (+ instruction-pointer 2) (get-mode modes 2))]
        (if (= value1 0)
          (assoc machine :ip (+ instruction-pointer 3))
          (assoc machine :ip new-ip)))
      (= opcode 6)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            new-ip (get-value machine (+ instruction-pointer 2) (get-mode modes 2))]
        (if (= value1 0)
          (assoc machine :ip new-ip)
          (assoc machine :ip (+ instruction-pointer 3))))
      (= opcode 7)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value machine (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value machine (+ instruction-pointer 3) 1)
            new-ip (+ instruction-pointer 4)
            new-memory (if (< value1 value2)
                         (set-value memory target 1)
                         (set-value memory target 0))]
        (assoc machine :memory new-memory :ip new-ip))
      (= opcode 8)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))
            value2 (get-value machine (+ instruction-pointer 2) (get-mode modes 2))
            target (get-value machine (+ instruction-pointer 3) 1)
            new-ip (+ instruction-pointer 4)
            new-memory (if (= value1 value2)
                         (set-value memory target 1)
                         (set-value memory target 0))]
        (assoc machine :memory new-memory :ip new-ip))
      (= opcode 9)
      (let [value1 (get-value machine (+ instruction-pointer 1) (get-mode modes 1))]
        (assoc machine :ip (+ instruction-pointer 2) :relative value1))
      :else "error")))

(defn run-opcode
  [machine]
   (let [new-machine (execute-opcode machine)]
     (cond
       (= (:ip new-machine) nil) new-machine
       (= new-machine "error") "error"
       :else (recur new-machine))))

(defn load-program
  [filename]
  (vec (map #(edn/read-string) (str/split (str/trim-newline (slurp (jio/resource filename))) #","))))

(defn make-machine
  ([memory]
   {:memory memory :ip 0 :input [] :output [] :relative 0})
  ([memory input]
   {:memory memory :ip 0 :input input :output [] :relative 0}))
