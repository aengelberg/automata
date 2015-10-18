(ns automata.core
  (:use loco.constraints
        loco.core)
  (:require [clojure.java.io :as io]
            [loco.automata :as a]))

(def sgb-words
  "5757 five-letter words (in order of popularity)
  http://www-cs-faculty.stanford.edu/~uno/sgb-words.txt"
  (->> (slurp (io/resource "sgb-words.txt"))
       (re-seq #"[a-z]+")))

(def all-words
  (->> (slurp (io/resource "US.dic"))
       (re-seq #"[a-z]+")))

(defn int->char
  [i]
  (char (+ i (int \a))))

(defn char->int
  [c]
  (- (int c) (int \a)))

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

(def N (count crossword-grid))
(def M (count (first crossword-grid)))

(def all-vars
  "All the variables used in this loco problem."
  (for [i (range N)
        j (range M)
        :when (= (get-in crossword-grid [i j]) white-char)]
    [:cell i j]))

(def base-model
  (for [v all-vars]
    ($in v (char->int \a) (char->int \z))))

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

(defn format-grid
  "Takes a Loco solution map and returns a grid of characters"
  [sol]
  (when sol
    (mapv vec
          (for [i (range N)]
            (for [j (range M)]
              (if (= white-char (get-in crossword-grid [i j]))
                (int->char (get sol [:cell i j]))
                \*))))))

(defn ppr
  "Pretty-prints a grid of letters"
  [grid]
  (doseq [row grid]
    (doseq [ch row]
      (print ch ""))
    (println)))

(defn dictionary->transitions
  [dict]
  (apply merge-with merge
         (for [word dict
               i (range (count word))]
           {(vec (take i word)) {(char->int (nth word i))
                                 (vec (take (inc i) word))}})))

(defn dictionary->automaton
  [dict]
  ;; Each state is a vector of characters:
  ;; [\d \o \g]
  ;; Accepting states are complete words.
  ;; Transitions are the integer of the next character: 
  ;; {[\d \o] {6 [\d \o \g]
  ;;           ...}
  ;;  ...}
  ;; Starting state is []
  (let [transitions (dictionary->transitions dict)
        accepting-states (for [word dict]
                           (vec word))]
    (a/map->automaton transitions
                      []
                      accepting-states)))

(def words-by-length (group-by count all-words))

(time
 (def automata-by-length
   "Creating a separate automaton for each length of word"
   (into {} (for [[k words] words-by-length]
              [k (dictionary->automaton words)]))))

(defn word-constraint
  [sequence]
  ($regular (automata-by-length (count sequence)) sequence))

(def crossword-model
  (concat base-model
          (map word-constraint (concat across down))))

(time
 (-> (solution crossword-model)
     format-grid
     ppr))
