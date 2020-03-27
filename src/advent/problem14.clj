(ns advent.problem14
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]
            [clojure.edn :as edn]))

(defn parse-recipe
  "Returns a vector [ingredient
                      {:product [ingredient amount]
                       :ingredients [[amount1 thing1] [amount2 thing2]]}
                    ]"
  [line]
  (let [[ingredient-string product-string] (str/split line #" => ")
        [product-amount product-name] (str/split product-string #" ")
        ingredients (str/split ingredient-string #", ")
        ingredient-vec (mapv #(str/split % #" ") ingredients)
        ingred-vec (mapv #(update % 0 edn/read-string) ingredient-vec)]
    [product-name {:product [product-name (edn/read-string product-amount)]
                   :ingredients ingred-vec}]))

(defn load-recipes
  [filename]
  (into {} (map parse-recipe (str/split-lines (slurp (jio/resource filename))))))

(defn recipe-product-amount
  [recipes ingredient]
  (second (:product (recipes ingredient))))

(defn ingredient-list
  [recipes ingredient]
  (:ingredients (recipes ingredient)))

(defn add-ingredient-to-list
  [recipes ingredients [amount ingredient]]
  (if (contains? ingredients ingredient)
    (update-in ingredients [ingredient 1] + amount)
    (assoc ingredients ingredient [0 amount])))

(defn update-ingredient
  [recipes ingredients ingredient]
  (let [stockpile (first (ingredients ingredient))
        requested (second (ingredients ingredient))
        recipe-amount (recipe-product-amount recipes ingredient)
        added-ingredients (ingredient-list recipes ingredient)]
    (if (>= stockpile requested)
      ingredients
      (update-in (reduce (partial add-ingredient-to-list recipes) ingredients added-ingredients) [ingredient 0] + recipe-amount))))

(defn update-ingredients
  [recipes ingredients]
  (let [new-ingredients (reduce
                         (partial update-ingredient recipes)
                         ingredients
                         (filter #(not= % "ORE") (keys ingredients)))]
    (if (= ingredients new-ingredients)
      ingredients
      (recur recipes new-ingredients))))

(defn problem14-1
  []
  (let [recipes (load-recipes "input14.txt")
        request {"FUEL" [0 1]}]
    (second ((update-ingredients recipes request) "ORE"))))
