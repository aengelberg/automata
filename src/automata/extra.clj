(ns automata.extra)




(require '[loco.automata :as a])
(def non-contiguous "0*1+0+1(0|1)*")

(defn max-subsequence
  "Finds the maximum sub-sequence in coll that matches a regex."
  [coll regex]
  (let [N (count coll)
        ;; each var "bit[i]" is 1 iff coll[i] is included in the
        ;; subsequence. We use [:bit i] as the variable names in the
        ;; Loco implementation.
        bit-vars (for [i (range N)]
                   [:bit i])
        bit-var-constraints (for [v bit-vars]
                              ($in v 0 1))
        regex-constraint ($regular (a/string->automaton regex)
                                   bit-vars)
        ;; we want to maximize bit[0] * coll[0] + bit[1] * coll[1] ...
        var-to-maximize ($scalar bit-vars coll)
        model (-> []
                  (into bit-var-constraints)
                  (conj regex-constraint))
        sol (solution model
                      :maximize var-to-maximize)]
    (when sol
      (let [bits (mapv #(sol [:bit %]) (range N))
            included-indexes (filter #(= 1 (sol [:bit %])) (range N))
            included-values (map (vec coll) included-indexes)]
        {:bits bits
         :subseq included-values
         :sum (apply + included-values)}))))












