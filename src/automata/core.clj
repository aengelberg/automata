(ns automata.core
  (:use loco.constraints
        loco.core)
  (:require [clojure.java.io :as io]
            [loco.automata :as a]
            [clojure.pprint :as pp]))














;;; SOLVING PROBLEMS WITH AUTOMATA, PART 2

(def part2
  ["How finite state machines have impacted Constraint Programming"
   
   "Code examples using Loco (my Constraint Programming library for Cloure)"])



























;;; WHIRLWIND TOUR OF CONSTRAINT PROGRAMMING

;; - CP is a new approach to Discrete Optimization
;; - Loco provides a Clojure DSL to "Finite-Domain Integer Constraint Programming"
;; - Translation: only variables with *integer* values and bounded domains.

;; Example problem: find all X, Y, Z in [1, 10000] where X + Y + Z = 5.

(defn find-solution-slow
  "This is inefficient."
  []
  (for [x (range 1 10000)
        y (range 1 10000)
        z (range 1 10000)
        :when (= 5 (+ x y z))]
    {:x x :y y :z z}))

#_(find-solution-slow) ; don't even bother



























(use 'loco.core)
(use 'loco.constraints)

(defn find-solution-fast
  "This is efficient."
  []
  (let [model [($in :x 1 10000)
               ($in :y 1 10000)
               ($in :z 1 10000)
               ($= 5 ($+ :x :y :z))]]
    (solutions model)))

(time (find-solution-fast))

;;; Propagators avoid dumb choices

;; - "X + Y + Z = 5" is a *propagator*
;; - It automagically "trims" the domains of variables
;;   to remove impossible values
;; - e.g. X, Y, and Z cannot be greater than 3

(comment
  ;; The initial model
  X in [1,10000]
  Y in [1,10000]
  Z in [1,10000]
  X + Y + Z = 5
  ;; Propagation magic: we know X, Y, and Z cannot go over 3 or
  ;; there's no way to satisfy the constraint.
  X in [1,3]
  Y in [1,3]
  Z in [1,3]
  X + Y + Z = 5
  ;; Now we do brute-force but with a much more reasonable model.
  ;; Case 1: X = 1
  X in [1]
  Y in [1,3]
  Z in [1,3]
  X + Y + Z = 5
  ;; Case 1.1: Y = 1
  X in [1]
  Y in [1]
  Z in [1,3]
  X + Y + Z = 5
  ;; Propagation magic: Z must be 3
  X in [1]
  Y in [1]
  Z in [3]
  X + Y + Z = 5
  ;; This is a valid solution! Return this and keep going
  ;; (backtracking if necessary).
  )

;; Key idea: Adding simple propagators in between search decisions is
;; very powerful.


































;; CP engines traditionally provide a rich set of constraints to model
;; problems.

;; - equality / inequality
($= :x :y) ($!= 5 :z) ($< :a 3)
;; - logical meta-constraints
($and ($= :x :y)
      ($or ($< :a 3)
           ($= :foo :baz)))
;; - more math goodies:
($= ($min [:a :b :c])
    ($max [:x :y :z]))
($= :x ($abs :y))

;; GLOBAL CONSTRAINTS: advanced properties that apply to a series of
;; variables.

;; - "distinct" 
($distinct [:x :y :z]) ; X, Y, and Z must have all different values
;; - "nth"
($= ($nth [1 2 3] :x) 3) ; X must be 2

;; Sometimes global constraints aren't enough



































;; See Loco's readme for an introduction to Constraint Programming.
;; https://github.com/aengelberg/loco#what-is-constraint-programming

;; The primary bottleneck to solving problems with CP is figuring out
;; how to declaratively express a problem with only the constraints
;; provided by the given CP library. Fortunately, many Constraint
;; Programming engines allow users to be designers of their own
;; constraints, through automata.

;; Here I will write a solver for a simplified crossword puzzle, in
;; which there are no word clues but I just have to fill the grid with
;; valid words from a dictionary.

;; I will use Loco, a Clojure library that wraps a Java library called
;; Choco. I will primarily demonstrate Loco's general-purpose
;; `$regular` constraint which applies an automaton to a list of
;; integer variables.

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

;; Loco only works with integers, so we must map all characters to
;; integers to represent a constraint problem, and convert back when
;; we display the solution. I'm avoiding `(int c)` and `(char i)`
;; because of https://github.com/chocoteam/choco3/issues/337

(defn int->char
  [i]
  (char (+ i (int \a))))

(defn char->int
  [c]
  (- (int c) (int \a)))

(def N (count crossword-grid))
(def M (count (first crossword-grid)))

;; Our variables in this model are `cell_i,j` for all i in (0,N-1) and
;; for all j in (0,M-1), such that the cell at i,j is an open space
;; (white cell) in the grid. We'll harness Loco's flexibility for
;; variable names, and make each variable name [:cell i j] for all i
;; and j.

(def all-vars
  "All the variables used in this loco problem."
  (for [i (range N)
        j (range M)
        :when (= (get-in crossword-grid [i j]) white-char)]
    [:cell i j]))

(def base-model
  ;; Basic constraint model with all the variables set up. TODO:
  ;; concat some more constraints to this and make a meaningful
  ;; constraint model.
  (for [v all-vars]
    ($in v (char->int \a) (char->int \z))))

;; The return value from Loco's solver won't be pretty, so here are
;; some utilities to display the solution in a human-friendly way:

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

(comment
  ;; Just to see if our base model works:
  (-> (solution base-model)
      format-grid
      ppr)
  ;; This prints out a crossword grid with all "a"s.
  )

;; Now to make the problem harder with some constraints...

(comment
  ;; It's also interesting to generate five-by-five grids of words,
  ;; and we can use Knuth's five-letter dictionary, and take some or
  ;; all of the words to adjust the commonness in the allowed
  ;; words. Exercise left to the reader.
  (def sgb-words
    "5757 five-letter words (in order of popularity)
  http://www-cs-faculty.stanford.edu/~uno/sgb-words.txt"
    (->> (slurp (io/resource "sgb-words.txt"))
         (re-seq #"[a-z]+"))))

(def all-words
  "118627 words of varying sizes. Contains very obscure words.
  (Also contains offensive words and slurs, coder discretion is advised)
  http://www.winedt.org/Dict/"
  (->> (slurp (io/resource "US.dic"))
       (re-seq #"[a-z]+")))

;; Here we will find all the segments of variables we need to fill
;; with valid words. "across" will find all the horizontal segments of
;; open spaces (white cells) in the grid between the walls (black
;; cells). "down" is similar but for vertical segments.

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

;; Somehow we have to efficiently express with constraints that a
;; sequence of variables must be one of thousands of possible words. A
;; naive approach would be something like this:

(defn word-constraint-slow
  [dictionary sequence]
  (apply $or
         (for [word dictionary
               :when (= (count word) (count sequence))]
           (apply $and
                  (for [[letter variable] (map vector word sequence)]
                    ($= variable (char->int letter)))))))

;; This is really slow:
(comment
  (time (-> (solution (concat base-model
                              (map (partial word-constraint-slow all-words)
                                   (concat across down))))
            format-grid
            ppr))
  ;; (doesn't even finish)
  )

;; Let's see if we can use the automaton constraint here. First we
;; have to create a deterministic finite automaton that accepts a
;; string iff it's a valid word in the dictionary. Fortunately this
;; isn't too hard. We will use loco's `map->automaton` function which
;; accepts state transitions as a map like so:
(comment
  ;; Example DFA - parses at least one 1
  (a/map->automaton
   {:q0 {1 :q1}
    :q1 {1 :q1}} ; transitions
   :q0 ; start state
   #{:q1})) ; accepting states

;; A notable aspect of Loco's state machines is that instead of input
;; symbols, they only parse integers (like everything else in
;; Loco).

;; Here is a sample DFA (which we will generalize soon) that parses
;; the dictionary #{"dog" "cat"}.

(comment
  (a/map->automaton
   {[] {(char->int \d) [\d]
        (char->int \c) [\c]}
    [\d] {(char->int \o) [\d \o]}
    [\d \o] {(char->int \g) [\d \o \g]}
    [\c] {(char->int \a) [\c \a]}
    [\c \a] {(char->int \t) [\c \a \t]}}
   []
   #{[\d \o \g] [\c \a \t]}))
;; Since I can use any Clojure data structure as the name of a state,
;; for clarity I am representing each state as a vector of the
;; characters that are in the word so far.

;; This is easily extendable to dictionaries with more words. Here is
;; the function, dictionary->automaton, that generalizes this:
(defn dictionary->transitions
  [dict]
  (apply merge-with merge
         (for [word dict
               i (range (count word))]
           {(vec (take i word)) {(char->int (nth word i))
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
  (let [_ (println "Generating automaton...")
        a (time (dictionary->automaton all-words))
        _ (println "Solving...")
        sol (time (solution (concat base-model
                                    (map (partial word-constraint-fast a)
                                         (concat across down)))))]
    (ppr (format-grid sol))))

(comment

  (def words-by-length (group-by count all-words))

  (time
   (def automata-by-length
     "Creating a separate automaton for each length of word"
     (into {} (for [[k words] words-by-length]
                [k (dictionary->automaton words)]))))

  (def crossword-model
    (concat base-model
            (map word-constraint (concat across down))))

  (time
   (-> (solution crossword-model)
       format-grid
       ppr))
  )
