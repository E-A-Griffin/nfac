;; Functions for converting front-end map of states/nodes for an NFA
;; to the equivalent back-end map of states/nodes for the NFA.
(ns nfac.front-to-back)

(defn out->transition
  "Convert a singular transition function nested inside :out to the
  equivalent representation nested inside :transition."
  [[k v] n]
  {:accepts v :begin-state n :end-state k})

(defn in->transition
  "Convert a singular transition function nested inside :in to the
  equivalent representation nested inside :transition."
  [[k v] n]
  {:accepts v :begin-state k :end-state n})

(defn get-start-node
  "Return the (first/only) start state/node from the front-end map of states."
  [front-struct]
  (some (fn [[k v]] (if (v :start?) k)) front-struct))

(defn single-node-front->back
  "Convert a singular front-end state/node to a singular back-end state/node."
  [kc vc m]
  (let [kc-str (name kc)] ;; kc-str: string representation of key
    (update-in
     (update-in
      (assoc-in
       (assoc-in
        (assoc-in m [kc :transitions]
                  (into []
                        (into #{}
                              (flatten
                               (concat
                                (for [[k v] (vc :out)]
                                  (map #(out->transition [k %] kc-str) v))
                                (for [[k v] (vc :in)]
                                  (map #(in->transition [k %] kc-str) v)))))))
       [kc :start?] (true? (get vc :start?)))
      [kc :final?] (true? (get vc :final?)))
     [kc] dissoc :in)
    [kc] dissoc :out)))


(defn all-nodes-front->back
  "Convert all front-end states/nodes to back-end states/nodes.
  Function to be called on map of front-end states/nodes to yield
  equivalent back-end map of states/nodes."
  [struct]
  {:start-node (get-start-node struct)
   :nodes
   (loop [remaining (rest struct)
          [kc vc]   (first struct)
          new-struct {}]
     (if (empty? kc)
       new-struct
       (recur (rest remaining) (first remaining)
              (single-node-front->back (keyword kc) vc new-struct))))})
