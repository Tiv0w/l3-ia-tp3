(ns tp3-ia.core)

(defonce problem [[[[4 3 8] [2 1]]
                   [[4 2 3]]
                   [[6 4] [7] [5 2]]]
                  [[[1 9 0]]
                   [[4 3] [0]]
                   [[2 8 4] [3 7] [5 4 1]]]])

(defonce α-β-pruning-call-count (atom 0))

(defn minimax
  "The Minimax algorithm.
  Give a truthy value to verbose to enable logging."
  ([tree maximizing] (minimax tree maximizing false))
  ([tree maximizing verbose]
   (when verbose (println "- Tree max:" tree maximizing))
   (if (number? tree)
     tree
     (if maximizing
       (apply max (map #(minimax % false verbose) tree))
       (apply min (map #(minimax % true verbose) tree))))))

(defn α-β-pruning
  "The Minimax algorithm with α-β pruning.
  A bit more complex than the basic Minimax, but might skip evaluation
  of some branches, thus being more performant.
  Give a truthy value to verbose to enable logging."
  ([tree maximizing]
   (α-β-pruning tree maximizing false))
  ([tree maximizing verbose]
   (reset! α-β-pruning-call-count 0)
   (α-β-pruning tree ##-Inf ##Inf maximizing verbose))
  ([tree α β maximizing verbose]
   (when verbose (println "- Tree α β max:" tree α β maximizing))
   (swap! α-β-pruning-call-count inc)
   (if (number? tree)
     tree
     (if maximizing
       (let [max-eval (atom ##-Inf)
             alpha (atom α)]
         (loop [children tree]
           (when (seq children)
             (swap! max-eval max (α-β-pruning (first children) @alpha β false verbose))
             (swap! alpha max @max-eval)
             (when (> β @alpha)
               (recur (rest children)))))
         @max-eval)

       (let [min-eval (atom ##Inf)
             beta (atom β)]
         (loop [children tree]
           (when (seq children)
             (swap! min-eval min (α-β-pruning (first children) α @beta true verbose))
             (swap! beta min @min-eval)
             (when (> @beta α)
               (recur (rest children)))))
         @min-eval)))))
