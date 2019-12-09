(ns advent.problem8
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(defn load-pixels
  [filename]
  (vec (map #(Character/digit (first %) 10) (partition 1 (slurp (jio/resource filename))))))

(defn make-layers
  [pixels x y]
  (partition (* x y) pixels))

(defn problem8-1
  []
  (let [layers (make-layers (load-pixels "input8.txt") 25 6)
        zero-layer (apply min-key #(count (filter (complement pos?) %)) layers)
        ones (count (filter #(= 1 %) zero-layer))
        twos (count (filter #(= 2 %) zero-layer))]
    (* ones twos)))

(defn decode-pixel
  [pixels]
  (let [pixel (first (drop-while #(= 2 %) pixels))]
    (if (= pixel 1) "X" " ")))

(defn decode-image
  ([layers] (decode-image layers []))
  ([layers output]
   (if (empty? (first layers))
     output
     (recur (map rest layers) (conj output (decode-pixel (map first layers)))))))

(defn print-image
  [pixels x y]
  (doseq [line (map str/join (partition x pixels))]
    (println line)))

(defn problem8-2
  []
  (print-image (decode-image (make-layers (load-pixels "input8.txt") 25 6)) 25 6))
