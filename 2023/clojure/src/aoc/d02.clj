(ns aoc.d02
  (:require
    [aoc.util :as util]
    )
  )

(defrecord GameLimits [red blue green]
  )

(defrecord BagPull [red blue green])

(defrecord Game [id pulls]
  )

(def limits (GameLimits. 12 14 13))

(defn parse-game-id
  [line]
  (let [game-string (first (clojure.string/split line #":"))]
    (Integer/parseInt (last (clojure.string/split game-string #" ")))
    )
  )

(defn filter-blank-strings
  [strings]
  (filter
    #(not (clojure.string/blank? %))
    strings
    )
  )

(defn build-color-pairs
  [string]
  (let [color-strings (clojure.string/split string #",")]
    (map
      filter-blank-strings
      (map
        #(clojure.string/split % #" ")
        color-strings
        )
      )
    )
  )

(defn get-value-for-color
  [color-pairs color]
  (let [matching-pair (first (filter #(= (last %) color) color-pairs))]
    (if (nil? matching-pair)
      0
      (Integer/parseInt (first matching-pair))
      )
    )
  )



(defn parse-pull
  [pull-string]
  (let [color-pairs (build-color-pairs pull-string)]
    (BagPull.
      (get-value-for-color color-pairs "red")
      (get-value-for-color color-pairs "blue")
      (get-value-for-color color-pairs "green")
      )
    )
  )

(defn parse-pulls
  [line]
  (let [pull-strings (last (clojure.string/split line #":"))]
    (map parse-pull (clojure.string/split pull-strings #";"))
    )
  )

(defn parse-game
  [line]
  (Game.
    (parse-game-id line)
    (parse-pulls line)
    )
  )

(defn any?
  [pred list]
  (nil?
    (first
      (filter
        pred
        list
        )
      )
    )
  )

(defn pull-is-possible?
  [pull]
  (cond
    (> (:red pull) (:red limits)) false
    (> (:blue pull) (:blue limits)) false
    (> (:green pull) (:green limits)) false
    :else true
    )
  )

(defn game-is-possible?
  [game]
  (any?
    #(not (pull-is-possible? %))
    (:pulls game))
  )

(defn p01
  [filename]
  (reduce
    +
    (map
      :id
      (filter
        game-is-possible?
        (map
          parse-game
          (
            util/readlines filename)
          )
        )
      ))
  )

(defn max-pull
  [game]
  (BagPull.
    (apply max (map :red (:pulls game)))
    (apply max (map :blue (:pulls game)))
    (apply max (map :green (:pulls game)))
    )
  )

(defn pull-power
  [pull]
  (* (:red pull) (:blue pull) (:green pull))
  )

(defn p02
  [filename]
  (reduce
    +
    (map
      pull-power
      (map
        max-pull
        (map
          parse-game
          (
            util/readlines filename)
          ))))
  )

(util/print-output "d02" p01 p02)
