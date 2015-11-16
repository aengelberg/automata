(ns automata.viz
  (:use rhizome.viz)
  (:require [clojure.string :as str]))

(defn viz-automaton
  [transition-map
   start-state
   final-states]
  (view-graph
   (distinct (apply concat
                    ["start"]
                    (keys transition-map)
                    (map vals (vals transition-map))))
   (into {"start" [start-state]}
         (for [[k v] transition-map]
           [k (distinct (vals v))]))
   :edge->descriptor
   (fn [src dest]
     (if (= src "start")
       {}
       (let [m (transition-map src)
             inputs (map key (filter #(= dest (val %))
                                     (seq m)))]
         {:label (str/join "," inputs)
          :style (when (some :dotted (map meta inputs))
                   "dashed")})))
   :node->descriptor (fn [n]
                       {:label n
                        :shape (cond
                                 (contains? final-states n)
                                 :doublecircle
                                 (= "start" n)
                                 :none
                                 :else
                                 :circle)})
   :options {:rankdir "LR"}))

(defn transform-inputs
  [transition-map f]
  (apply merge-with merge
         (for [[src m] transition-map
               [i dest] m]
           {src {(f i) dest}})))
