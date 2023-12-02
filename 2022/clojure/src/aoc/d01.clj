(ns aoc.d01
  (:require
    [aoc.util :as util]
    [clojure.string]
    )
  )

(defn read-input
  [filename]
  (map
    #(if (clojure.string/blank? %)
       nil
       (Integer/parseInt %))
    (util/readlines filename))
  )

(defn add-to-first-in-list-or-add-new-item
  "Adds the input to first item in the list, or appends a new 0 to the front of the list if [input] is null"
  [sums input]
  (if (nil? input)
    (conj sums 0)
    (conj (rest sums) (+ input (first sums)))
    ))

(defn parse-calories
  [input]
  (
    reduce
    add-to-first-in-list-or-add-new-item
    [0]
    input
    )
  )

(defn p01
  [filename]
  (apply max (parse-calories (read-input filename)))
  )

(defn p02
  [filename]
  (
    reduce
    +
    (take-last 3 (sort (parse-calories (read-input filename)))))
  )

(util/print-output
  "d01"
  p01
  p02
  )
