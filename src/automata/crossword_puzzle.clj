(ns automata.crossword-puzzle
  (:use loco.constraints
        loco.core)
  (:require [clojure.java.io :as io]
            [loco.automata :as a]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [automata.viz :refer
             [viz-dictionary-transitions]]))

;; Problem: Fill a crossword-puzzle-style grid with letters.

;; CONSTRAINT: Each horizontal or vertical chunk of letters must be a
;; valid word. Just like a crossword puzzle.

;; Here is the grid to fill in:
;; http://www.webcrosswords.com/images/crossword_puzzle.gif
(def crossword-grid
  '[[- - - - * - - - - * * - - - -]
    [- - - - * - - - - - * - - - -]
    [- - - - * - - - - - * - - - -]
    [- - - - - - - * - - - - - - -]
    [* - - - - * * - - - - * * * *]
    [* * * * - - - - - * - - - - -]
    [* - - - * - - - * - - - - - -]
    [- - - - * - - - - - * - - - -]
    [- - - - - - * - - - * - - - *]
    [- - - - - * - - - - - * * * *]
    [* * * * - - - - * * - - - - *]
    [- - - - - - - * - - - - - - -]
    [- - - - * - - - - - * - - - -]
    [- - - - * - - - - - * - - - -]
    [- - - - * * - - - - * - - - -]])

(def white-char '-)
(def black-char '*)

(def all-words
  "118627 words of varying sizes. Contains very obscure words."
  (->> (slurp (io/resource "US.dic"))
       (re-seq #"[a-z]+")))

(def N (count crossword-grid))
(def M (count (first crossword-grid)))

;; VARIABLES:
;; [:cell i j] = the ascii value of the letter at i,j in the grid

(def all-vars
  (for [i (range N)
        j (range M)
        :when (= (get-in crossword-grid [i j]) white-char)]
    [:cell i j]))

(def base-model
  (for [v all-vars]
    ($in v (int \a) (int \z))))

;; Printing utilities

(defn format-grid
  "Takes a Loco solution map and returns a grid of characters"
  [sol]
  (when sol
    (mapv vec
          (for [i (range N)]
            (for [j (range M)]
              (if (= white-char (get-in crossword-grid [i j]))
                (char (get sol [:cell i j]))
                \*))))))

(defn ppr
  "Pretty-prints a grid of letters"
  [grid]
  (doseq [row grid]
    (doseq [ch row]
      (print ch ""))
    (println)))

(comment
  ;; Just to see if our base model works:
  (-> (solution base-model)
      format-grid
      ppr)
  ;; This prints out a crossword grid with all "a"s.
  )

;; Now to add constraints

(def across
  "All sequences of variables that spell out an \"across\" answer."
  (for [i (range N)
        j (range M)
        ;; this cell is white
        :when (= (get-in crossword-grid [i j]) white-char)
        ;; previous cell is not white (we're not in the middle of a word)
        :when (not= (get-in crossword-grid [i (dec j)]) white-char)
        ;; next cell is white (so the word is at least 2 letters long)
        :when (= (get-in crossword-grid [i (inc j)]) white-char)]
    (for [j2 (range j M)
          :while (= (get-in crossword-grid [i j2]) white-char)]
      [:cell i j2])))

(comment
  (take 5 across)
  =>
  (([:cell 0 0] [:cell 0 1] [:cell 0 2] [:cell 0 3])
   ([:cell 0 5] [:cell 0 6] [:cell 0 7] [:cell 0 8])
   ([:cell 0 11] [:cell 0 12] [:cell 0 13] [:cell 0 14])
   ([:cell 1 0] [:cell 1 1] [:cell 1 2] [:cell 1 3])
   ([:cell 1 5] [:cell 1 6] [:cell 1 7] [:cell 1 8] [:cell 1 9])))

(def down
  "All sequences of variables that spell out a \"down\" answer."
  (for [j (range M)
        i (range N)
        ;; this cell is white
        :when (= (get-in crossword-grid [i j]) white-char)
        ;; previous cell is not white (we're not in the middle of a word)
        :when (not= (get-in crossword-grid [(dec i) j]) white-char)
        ;; next cell is white (so the word is at least 2 letters long)
        :when (= (get-in crossword-grid [(inc i) j]) white-char)]
    (for [i2 (range i N)
          :while (= (get-in crossword-grid [i2 j]) white-char)]
      [:cell i2 j])))

;; THE WORD CONSTRAINT - NAIVE APPROACH

(defn word-constraint-slow
  [dictionary sequence]
  (apply $or
         (for [word dictionary
               :when (= (count word) (count sequence))]
           (apply $and
                  (for [[letter variable] (map vector word sequence)]
                    ($= variable (int letter)))))))

;; This is really slow:
(comment
  (time (-> (solution (concat base-model
                              (map (partial word-constraint-slow all-words)
                                   (concat across down))))
            format-grid
            ppr))
  ;; (doesn't even finish)
  )




;; General functions to construct dictionary automata
(defn dictionary->transitions
  [dict]
  (apply merge-with merge
         (for [word dict
               i (range (count word))]
           {(vec (take i word)) {(int (nth word i))
                                 (vec (take (inc i) word))}})))
(defn dictionary->automaton
  [dict]
  (let [transitions (dictionary->transitions dict)
        accepting-states (for [word dict]
                           (vec word))]
    (a/map->automaton transitions
                      []
                      accepting-states)))

;; Now we can constrain this automaton onto each sequence of vars:

(defn word-constraint-fast
  [automaton sequence]
  ($regular automaton sequence))

(defn solve-crossword-with-automaton
  []
  (let [_ (println "Generating automata...")
        all-word-lengths (distinct (map count all-words))
        automaton-per-length (time (into {} (for [i all-word-lengths]
                                              [i (dictionary->automaton
                                                  (filter #(= (count %) i)
                                                          all-words))])))
        _ (println "Solving...")
        sol (time (solution (concat base-model
                                    (map #(word-constraint-fast
                                           (automaton-per-length (count %))
                                           %)
                                         (concat across down)))))]
    (ppr (format-grid sol))))
