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

(defrecord Coords
  [x y])

(defrecord SymbolCoords
  [symbol coords]
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

(defn get-indexed-symbols
  [indexed-chars]
  (filter
    #(is-symbol (:char %))
    indexed-chars
    )

  )


(defn get-symbol-coords
  [indexed-symbol-lists]
  (flatten (filter
             #(not (empty? %))
             (map-indexed
               (fn [idx indexed-symbol-list]
                 (map
                   (fn [indexed-symbol] (SymbolCoords. (:char indexed-symbol) (Coords. (:index indexed-symbol) idx)))
                   indexed-symbol-list
                   )
                 )
               indexed-symbol-lists
               ))))

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
                           (fn [y] (Coords. x y))
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
      (let [symbol-coords (get-symbol-coords (map get-indexed-symbols indexed-lines))]
        (reduce
          +
          (
            map
            #(:value (:part-number %))
            (filter
              #(touching-symbol? % (map :coords symbol-coords))
              part-number-coords
              )
            )
          )
        )
      )
    )
  )

(defn get-surrounding-coords-of-symbol-coord
  [symbol-coord]
  ;hack this into a partnumbercoord to use the same function
  (get-surrounding-coords (PartNumberCoord. (:y (:coords symbol-coord)) (PartNumber. 0 (:x (:coords symbol-coord)) (:x (:coords symbol-coord)))))
  )

(defn get-touching-numbers
  [symbol-coord part-number-coords]
  (filter
    (fn [part-number-coord]
      (
        touching-symbol?
        part-number-coord
        (list (:coords symbol-coord))
        )
      )
    part-number-coords
    )
  )

(defn get-lists-of-size-2
  [lists]
  (filter
    #(= (count %) 2)
    lists
    )
  )

(defn product-of-list-of-part-number-coords
  [part-number-coords]
  (
    reduce
    *
    (map
      #(:value (:part-number %))
      part-number-coords
      )
    )
  )

(defn p02
  [filename]
  (let [indexed-lines (map parse-indexed-characters (util/readlines filename))]
    (let [part-number-coords (index-part-numbers (map parse-part-numbers-in-line indexed-lines))]
      (let [symbol-coords (get-symbol-coords (map get-indexed-symbols indexed-lines))]
        (let [gear-symbols (filter #(= \* (:symbol %)) symbol-coords)]
          (let [touching-number-lists (map #(get-touching-numbers % part-number-coords) gear-symbols)]
            (reduce
              +
              (
                map
                product-of-list-of-part-number-coords
                (get-lists-of-size-2 touching-number-lists))
              )
            ))
        )
      )
    )
  )

(util/print-output "d03" p01 p02)
