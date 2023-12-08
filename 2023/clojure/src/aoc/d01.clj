(ns aoc.d01
  (:require
    [aoc.util :as util]
    )
  )

(def lookupTable
  {
   "one"   "1",
   "two"   "2",
   "three" "3",
   "four"  "4",
   "five"  "5",
   "six"   "6",
   "seven" "7",
   "eight" "8",
   "nine"  "9"
   }
  )

(defn
  take-first-where
  [pred list]
  (first
    (filter
      pred
      list
      ))
  )

(defn
  get-first-and-last-digits
  [line]
  (map str
       (list (take-first-where #(Character/isDigit %) line) (take-first-where #(Character/isDigit %) (reverse line)))
       )
  )

(defn pair-to-int
  [pair]
  (Integer/parseInt (str (first pair) (last pair)))
  )

(defn get-calibration-value
  [line]
  (pair-to-int (get-first-and-last-digits line))
  )

(defn get-number-str-in-substr
  [substr]
  (first
    (remove nil?
            (map
              #(if (clojure.string/includes? substr (key %))
                 (val %)
                 nil
                 )
              lookupTable
              ))
    ))

(defn p01
  [filename]
  (reduce
    +
    (map
      get-calibration-value
      (util/readlines filename)))
  )

(defn get-number-in-line
  [line]
  (first (filter #(not (nil? %)) (list
                                   (take-first-where #(Character/isDigit %) line)
                                   (get-number-str-in-substr line)
                                   )))
  )

(defn get-first-number-in-line
  [line depth]
  (let [found (get-number-in-line (apply str (take depth line)))]
    (if (nil? found)
      (get-first-number-in-line line (+ depth 1))
      found
      )
    )
  )

(defn get-last-number-in-line
  [line depth]
  (let [found (
                get-number-in-line (apply str (take-last depth line))
                                   )]
    (if (nil? found)
      (get-last-number-in-line line (+ depth 1))
      found)
    )
  )

(defn get-first-and-last-digits-p2
  [line]
  (map str
       (list (get-first-number-in-line line 0) (get-last-number-in-line line 0))
       )
  )

(defn get-calibration-value-p2
  [line]
  (pair-to-int (get-first-and-last-digits-p2 line))
  )

(defn p02
  [filename]
  (reduce
    +
    (map
      get-calibration-value-p2
      (util/readlines filename)
      )))


(util/print-output "d01" p01 p02)
