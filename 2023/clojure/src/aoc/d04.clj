(ns aoc.d04
  (:require
    [aoc.util :as util]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    )
  )

(s/valid? even? 10)
(s/def :d04/game-number int?)
(s/def :d04/winning-numbers (s/coll-of int?))
(s/def :d04/played-numbers (s/coll-of int?))
(s/def :d04/game (s/keys :req-un [:d04/game-number :d04/winning-numbers :d04/played-numbers]))
(s/def :d04/won-game-count int?)
(s/def :d04/game-result (s/keys :req-un [:d04/game-number :d04/won-game-count]))

(defrecord Game
  [game-number winning-numbers played-numbers])

(defrecord GameResult
  [game-number won-game-count])

(defn
  parse-game-number
  [line]
  {:pre  [(s/valid? string? line)]
   :post [(s/valid? int? %)]}
  (let [pre-colon (first (string/split line #":"))]
    (Integer/parseInt (last (string/split pre-colon #" ")))))


(defn parse-winning-numbers
  [line]
  {:pre  [(s/valid? string? line)]
   :post [(s/valid? (s/coll-of int?) %)]}
  (let [post-colon (last (string/split line #":"))]
    (let [pre-bar (first (string/split post-colon #"\|"))]
      (map
        #(Integer/parseInt %)
        (filter #(not (string/blank? %)) (string/split pre-bar #"\s")))
      )
    )
  )

(defn parse-played-numbers
  [line]
  {:pre  [(s/valid? string? line)]
   :post [(s/valid? (s/coll-of int?) %)]}
  (let [post-colon (last (string/split line #":"))]
    (let [post-bar (last (string/split post-colon #"\|"))]
      (map
        #(Integer/parseInt %)
        (filter #(not (string/blank? %)) (string/split post-bar #"\s")))
      )
    )
  )

(defn
  parse-game
  [line]
  {:pre  [(s/valid? string? line)]
   :post [(s/valid? :d04/game %)]}
  (Game.
    (parse-game-number line)
    (parse-winning-numbers line)
    (parse-played-numbers line)
    )
  )

(defn list-contains-number
  [list number]
  {:pre  [(s/and (s/valid? (s/coll-of int) list) (s/valid? int? number))]
   :post [(s/valid? boolean? %)]
   }
  (not (empty?
         (filter
           (fn [itm] (= itm number))
           list)
         )
       )
  )


(defn get-winning-numbers
  [game]
  {:pre  [(s/valid? :d04/game game)]
   :post [(s/valid? (s/coll-of int?) %)]}
  (filter
    #(list-contains-number (:winning-numbers game) %)
    (:played-numbers game)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn get-point-value-of-list
  [nums]
  {:pre  [(s/valid? (s/coll-of int?) nums)]
   :post [(s/valid? int? %)]}
  (let [n (count nums)]
    (if (> n 0)
      (exp 2 (- n 1))
      0
      )
    )
  )

(defn p01
  [filename]
  {:pre  [(s/valid? string? filename)]
   :post [(s/valid? int? %)]}
  (reduce
    +
    (map
      #(get-point-value-of-list (get-winning-numbers (parse-game %)))
      (util/readlines filename)
      )
    )
  )

(defn get-game-result
  [game]
  {:pre  [(s/valid? :d04/game game)]
   :post [(s/valid? :d04/game-result %)]}
  (GameResult.
    (:game-number game)
    (count (get-winning-numbers game))
    )
  )

(defn add-game-counts-to-map
  [count-map game-result]
  {:pre  [(s/and (s/valid? (s/map-of int? int?) count-map) (s/valid? :d04/game-result game-result))]
   :post [(s/valid? (s/map-of keyword? int?) %)]}
  (let [number-of-game-cards (get count-map (keyword (str (:game-number game-result))))]
    (let [games-to-apply-to (map
                              #(+ % (+ 1 (:game-number game-result)))
                              (range 0 (:won-game-count game-result)))]
      (reduce
        (fn [curr-map game-number-to-add-count-for]
          (let [existing-count (get count-map (keyword (str game-number-to-add-count-for)) 1)]
            (assoc curr-map (keyword (str game-number-to-add-count-for)) (+ number-of-game-cards existing-count))
            )
          )
        count-map
        games-to-apply-to)
      )
    )
  )

(defn build-base-game-count-map
  [game-results]
  {:pre  [(s/valid? (s/coll-of :d04/game-result) game-results)]
   :post [(s/valid? (s/map-of keyword? int?) %)]}
  (reduce
    (fn [map game-result] (assoc map (keyword (str (:game-number game-result))) 1))
    {}
    game-results
    )
  )

(defn p02
  [filename]
  {:pre  [(s/valid? string? filename)]
   :post [(s/valid? int? %)]}
  (let [game-results (map
                       #(get-game-result (parse-game %))
                       (util/readlines filename)
                       )]
    (reduce
      +
      (vals
        (reduce
          add-game-counts-to-map
          (build-base-game-count-map game-results)
          game-results
          )
        )
      )
    )
  )

(util/print-output "d04" p01 p02)
