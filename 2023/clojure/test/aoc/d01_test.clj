(ns aoc.d01-test
  (:require [clojure.test :refer :all]))

(deftest get-first-and-last
  (is (= '("1" "2") (aoc.d01/get-first-and-last-digits "1abc2")))
  (is (= '("1" "5") (aoc.d01/get-first-and-last-digits "a1b2c3d4e5f")))
  )
