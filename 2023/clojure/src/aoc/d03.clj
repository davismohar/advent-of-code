(ns aoc.d03
  (:require
    [aoc.util :as util]
    )
  )

(defrecord IndexedCharacter
  [char index]
  )

(defn parse-indexed-characters
  [line]
  (map-indexed
    (fn [idx itm] (IndexedCharacter. itm idx))
    line
    )
  )

(defrecord PartialPartNumber
  [valueString start end]
  )

(defrecord PartNumber
  [value start end]
  )

(defn take-next-number
  [indexed-line]
  (cond
    (zero? (count indexed-line)) nil
    (Character/isDigit (:char (first indexed-line))) (let [digit-indexed-chars (take-while #(Character/isDigit (:char %)) indexed-line)]
                                                       (PartNumber.
                                                         (Integer/parseInt (apply str (
                                                                                        map
                                                                                        :char
                                                                                        digit-indexed-chars
                                                                                        )))
                                                         (:index (first digit-indexed-chars))
                                                         (:index (last digit-indexed-chars))
                                                         )
                                                       )
    :else (take-next-number (rest indexed-line))
    )
  )

(defn parse-part-numbers
  [indexed-line parsed]
  (
    cond
    (nil? indexed-line) parsed
    :else (let [next-num (take-next-number indexed-line)]
            (if (nil? next-num)
              parsed
              (parse-part-numbers (filter #(> (:index %) (:end next-num)) indexed-line) (conj parsed next-num))
              )
            )
    )
  )

(defn parse-part-numbers-in-line
  [indexed-lines]
  (parse-part-numbers indexed-lines '())
  )

(defrecord PartNumberCoord
  [y part-number]
  )

(defn index-part-numbers
  [part-number-lists]
  (flatten (filter
             #(not (empty? %))
             (map-indexed
               (fn [idx itm]
                 (map
                   #(PartNumberCoord. idx %)
                   itm
                   ))
               part-number-lists
               )))
  )

(defn
  is-symbol
  [char]
  (cond
    (= \. char) false
    (Character/isDigit char) false
    :else true
    ))

(defn get-symbol-indexes
  [indexed-chars]
  (map
    :index
    (filter
      #(is-symbol (:char %))
      indexed-chars
      )
    )
  )

(defrecord SymbolCoords
  [x y]
  )

(defn get-symbol-coords
  [indexed-char-lists]
  (flatten (filter
             #(not (empty? %))
             (map-indexed
               (fn [idx itm]
                 (map
                   #(SymbolCoords. % idx)
                   itm
                   )
                 )
               indexed-char-lists
               )))
  )

(defn y-range
  [part-number-coord]
  (range (- (:y part-number-coord) 1) (+ (:y part-number-coord) 2))
  )

(defn x-range
  [part-number-coord]
  (let [part-number (:part-number part-number-coord)]
    (range (- (:start part-number) 1) (+ (:end part-number) 2))
    )
  )

(defn get-surrounding-coords
  [part-number-coord]
  (let [y-range (y-range part-number-coord)]
    (let [x-range (x-range part-number-coord)]
      (flatten (map
                 (fn [x] (
                           map
                           (fn [y] (SymbolCoords. x y))
                           y-range
                           ))
                 x-range))
      )
    )
  )


(defn touching-symbol?
  [part-number-coord symbol-coords]
  (let [surrounding-coords (get-surrounding-coords part-number-coord)]
    (not (empty? (flatten (filter
                            not-empty
                            (map
                              (fn
                                [surrounding-coord]
                                (filter
                                  (fn
                                    [symbol-coord]
                                    (and (= (:x surrounding-coord) (:x symbol-coord))
                                         (= (:y surrounding-coord) (:y symbol-coord))
                                         )
                                    )
                                  symbol-coords
                                  )
                                )
                              surrounding-coords
                              )
                            )
                          )
                 )
         )
    )
  )


(defn p01
  [filename]
  (let [indexed-lines (map parse-indexed-characters (util/readlines filename))]
    (let [part-number-coords (index-part-numbers (map parse-part-numbers-in-line indexed-lines))]
      (let [symbol-coords (get-symbol-coords (map get-symbol-indexes indexed-lines))]
        (reduce
          +
          (
            map
            #(:value (:part-number %))
            (filter
              #(touching-symbol? % symbol-coords)
              part-number-coords
              )
            )
          )
        )
      )
    )
  )

(defn p02
  [filename]
  nil
  )

(util/print-output "d03" p01 p02)
