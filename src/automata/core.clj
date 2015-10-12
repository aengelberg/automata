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

(defn int->char
  [i]
  (char (+ i (int \a))))

(defn char->int
  [c]
  (- (int c) (int \a)))

(def all-vars
  "All the variables used in this loco problem."
  (for [i (range 5)
        j (range 5)]
    [:cell i j]))

(def rows (partition 5 all-vars))
(def cols (apply map list rows))

(def base-model
  (for [v all-vars]
    ($in v (char->int \a) (char->int \z))))

(defn format-grid
  "Takes a Loco solution map and returns a grid of characters"
  [sol]
  (when sol
    (mapv vec
          (for [row rows]
            (for [v row]
              (int->char (get sol v)))))))

(defn ppr
  "Pretty-prints a grid of letters"
  [grid]
  (doseq [row grid]
    (doseq [ch row]
      (print ch ""))
    (println)))

(defn word->regex
  "Formats a word from the dictionary to be consumed by
  a/string->automaton"
  [word]
  (apply str (for [ch word]
               (str "<" (char->int ch) ">"))))

(defn reduce-tree
  [f l]
  (let [c (count l)
        half (/ c 2)]
    (case c
      1 (first l)
      2 (apply f l)
      (f (reduce-tree f (take half l))
         (reduce-tree f (drop half l))))))

(time
 (def automaton
   (-> (->> sgb-words
            (map word->regex)
            (map a/string->automaton)
            doall
            (reduce-tree (fn [a1 a2]
                           (doto (a/union a1 a2)
                             .minimize))))))) ; TODO: expose .minimize in Loco

(defn word-constraint
  [sequence]
  ($regular automaton sequence))

(def crossword-model
  (concat base-model
          (map word-constraint (concat rows cols))
          [($!= [:cell 0 4] [:cell 4 0])]))

(time
 (-> (solution crossword-model)
     format-grid
     ppr))
