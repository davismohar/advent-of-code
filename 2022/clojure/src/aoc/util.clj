(ns aoc.util)

(defn readlines
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr)))
  )

(defn print-output
  [day fn1 fn2]
  (println (str "-- " day " --"))
  (println (str "test p01: " (fn1 (str "inputs/test/" day ".txt"))))
  (println (str "real p01: " (fn1 (str "inputs/real/" day ".txt"))))
  (println (str "test p02: " (fn2 (str "inputs/test/" day ".txt"))))
  (println (str "real p02: " (fn2 (str "inputs/real/" day ".txt"))))
  )
