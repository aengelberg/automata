(ns automata.crossword-puzzle-5x5
  (:use loco.constraints
        loco.core)
  (:require [clojure.java.io :as io]
            [loco.automata :as a]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [automata.viz :refer
             [viz-automaton]]))

;; Problem: Fill a 5x5 grid with letters.

;; CONSTRAINT: Each row or column must spell a valid word.

(def all-words
  "The 2000 most common 5-letter words."
  (->> (slurp (io/resource "sgb-words.txt"))
       (re-seq #"[a-z]+")
       (take 2000)))



;; VARIABLES: [:cell i j] for all i and j in [0,4].

;; Each value is the ASCII code of the character it
;; represents in the grid.

;; The starting domain is [97,122].

(def all-vars
  (for [i (range 5)
        j (range 5)]
    [:cell i j]))

(def base-model
  (for [v all-vars]
    ($in v (int \a) (int \z))))



;; The word constraint - slow approach.

(comment
  ;; slow approach: sketch
  ($or ($and ($= [:cell 0 0] (int \w))
             ($= [:cell 0 1] (int \h))
             ($= [:cell 0 2] (int \e))
             ($= [:cell 0 3] (int \r))
             ($= [:cell 0 4] (int \e)))
       ($and ...)
       ...))

(defn slow-word-constraint
  [cells]
  (apply $or
         (for [possible-word all-words]
           (apply $and
                  (for [[cell character]
                        (map vector
                             cells
                             possible-word)]
                    ($= cell (int character)))))))





;; The word constriant - fast approach.

;; Sample DFA (parses the dictionary #{"dog" "cat"})
(def sample-dictionary-transitions
  {"" {(int \d) "d"
       (int \c) "c"}
   "d" {(int \o) "do"
        (int \u) "du"}
   "do" {(int \g) "dog"}
   "c" {(int \a) "ca"}
   "ca" {(int \t) "cat"}
   "du" {(int \c) "duc"}
   "duc" {(int \k) "duck"}})
(def sample-dictionary-automaton
  (a/map->automaton
    sample-dictionary-transitions
    ""
    #{"dog" "cat" "duck"}))

;; General functions to construct dictionary automata
(defn dictionary->transitions
  [dict]
  (apply merge-with merge
         (for [word dict
               i (range (count word))]
           {(subs word 0 i)
            {(int (nth word i))
             (subs word 0 (inc i))}})))
(defn dictionary->automaton
  [dict]
  (let [transitions (dictionary->transitions dict)
        accepting-states (set dict)]
    (a/map->automaton transitions
                      ""
                      accepting-states)))

(comment ; ignore this
  (viz-automaton
   (apply merge-with merge
          (for [[src m] (dictionary->transitions ["dog" "cat" "duck"])
                [i dest] m]
            {src {(char i) dest}}))
   ""
   #{"dog" "cat" "duck"}))

(defn fast-word-constraint
  [automaton sequence]
  ($regular automaton sequence))

(declare format-grid ppr)
(defn solve-crossword-with-automaton
  []
  (let [_ (println "Generating automaton...")
        a (dictionary->automaton all-words)
        _ (println "Solving puzzle...")
        model (concat base-model
                      ;; add an element of asymmetry
                      ;; so we don't get a
                      ;; fully symmetric solution
                      [($!= [:cell 0 4] [:cell 4 0])]
                      (for [row (partition 5 all-vars)]
                        (fast-word-constraint a row))
                      (for [col (apply map vector
                                       (partition 5 all-vars))]
                        (fast-word-constraint a col)))]
    (ppr (format-grid (solution model)))))

(defn format-grid
  "Takes a Loco solution map and returns a grid of characters"
  [sol]
  (when sol
    (mapv vec
          (for [i (range 5)]
            (for [j (range 5)]
              (char (get sol [:cell i j])))))))

(defn ppr
  "Pretty-prints a grid of letters"
  [grid]
  (doseq [row grid]
    (doseq [ch row]
      (print ch ""))
    (println)))
