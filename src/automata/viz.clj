(ns automata.viz
  (:use rhizome.viz)
  (:require [clojure.string :as str]))

(defn viz-transition-map
  "Takes a transition map, and a function that takes some input
  character and returns a prettier version to display."
  ([transition-map input-transform]
   (view-graph
    (distinct (apply concat
                     (keys transition-map)
                     (map vals (vals transition-map))))
    (into {} (for [[k v] transition-map]
               [k (distinct (vals v))]))
    :edge->descriptor
    (fn [src dest]
      (let [m (transition-map src)
            inputs (map key (filter #(= dest (val %))
                                    (seq m)))
            inputs (map input-transform inputs)]
        {:label (str/join "," inputs)}))
    :node->descriptor (fn [n] {:label n})
    :options {:rankdir "LR"}))
  ([transition-map]
   (viz-transition-map transition-map identity)))

(defn viz-dictionary-transitions
  [dict-transitions]
  (viz-transition-map
   (into {} (for [[k m] dict-transitions]
              [(apply str k)
               (into {} (for [[i v] m]
                          [(char i) (apply str v)]))]))))
