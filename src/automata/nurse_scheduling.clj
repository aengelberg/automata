(ns automata.nurse-scheduling
  (:use loco.constraints
        loco.core)
  (:require [clojure.java.io :as io]
            [loco.automata :as a]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [automata.viz :refer [viz-automaton
                                  transform-inputs]]))

;; (This example can be found on the MiniZinc tutorial:
;; http://www.minizinc.org/downloads/doc-latest/minizinc-tute.pdf)

;; Problem: Determine seven nurses' schedules across 10 days.

;; Each nurse can have a day shift, or night shift, or no shifts on a
;; given day.

;; CONSTRAINT #1: There must be 3 nurses on day shifts and 2 nurses on
;; night shifts on any given date.
;; CONSTRAINT #2: A nurse can't work more than 3 dates in a row.
;; CONSTRAINT #3: A nurse can't do more than 2 night shifts in a row.

(comment
  ;; Example solution:
  +--------+--+--+--+--+--+--+--+--+--+--+
  |        |D0|D1|D2|D3|D4|D5|D6|D7|D8|D9|
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 0 |d |d |d |  |d |d |d |  |n |n |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 1 |d |d |d |  |d |d |d |  |n |n |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 2 |d |d |  |d |d |n |  |n |d |d |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 3 |n |n |  |d |n |d |  |d |d |d |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 4 |n |  |d |n |n |  |d |d |d |  |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 5 |  |n |n |d |  |n |n |d |  |d |
  +--------+--+--+--+--+--+--+--+--+--+--+
  |Nurse 6 |  |  |n |n |  |  |n |n |  |  |
  +--------+--+--+--+--+--+--+--+--+--+--+
  ;; (d = 1, n = 2, blank = 0)
  )

(def n-nurses 7)
(def n-days 10)

(def minimum-day-shifts 3)
(def minimum-night-shifts 2)

;; VARIABLES:
;; [:shift i j] = what kind of shift nurse i does on day j
;; 1 = day, 2 = night, 0 = nothing

;; there are 7 (nurses) * 10 (days) = 70 variables.-

(def day 1)
(def night 2)
(def nothing 0)

(def shift-transition-map
  {:q1 {nothing :q1
        day :q2
        night :q3}
   :q2 {nothing :q1
        day :q4
        night :q4}
   :q3 {nothing :q1
        day :q4
        night :q5}
   :q4 {nothing :q1
        day :q6
        night :q6}
   :q5 {nothing :q1
        day :q6}
   :q6 {nothing :q1}})

(def shift-automaton
  (a/map->automaton
   shift-transition-map
   :q1
   #{:q1 :q2 :q3 :q4 :q5 :q6}))

(comment
  (viz-automaton
   (transform-inputs shift-transition-map
                     {night 'n
                      day 'd
                      nothing (with-meta 'o {:dotted true})})
   :q1
   #{:q1 :q2 :q3 :q4 :q5 :q6}))

(defn all-shift-vars []
  (for [n (range n-nurses)
        d (range n-days)]
    [:shift n d]))

(defn shift-var-declarations []
  (for [v (all-shift-vars)]
    ($in v [nothing day night])))

(defn nurse-constraint
  [nurse-id]
  ;; This automaton covers constraint #2 and #3
  (let [row (for [d (range n-days)]
              [:shift nurse-id d])]
    ($regular shift-automaton row)))

(defn day-constraint
  [day-id]
  (let [column (for [n (range n-nurses)]
                 [:shift n day-id])]
    ($cardinality column {day minimum-day-shifts
                          night minimum-night-shifts
                          nothing (- n-nurses minimum-day-shifts minimum-night-shifts)}
                  :closed true)))

(defn solve-nurse-shifts
  []
  (let [model (concat (shift-var-declarations)
                      (map nurse-constraint (range n-nurses))
                      (map day-constraint (range n-days)))
        sol (solution model)]
    (when sol
      (println
       (str/join "\n"
         (for [n (range n-nurses)]
           (str/join " "
             (for [d (range n-days)]
               ({nothing '-, day 'd, night 'n}
                (sol [:shift n d]))))))))))
