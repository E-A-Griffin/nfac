(ns dfac.front-to-back)

(defn out->transition
  [[k v] n]
  {:accepts v :begin-state n :end-state k})

(defn in->transition
  [[k v] n]
  {:accepts v :begin-state k :end-state n})

(defn get-start-node
  [front-struct]
  (some (fn [[k v]] (if (v :start?) k)) front-struct))

(defn single-front->back
  [kc vc m]
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
                                (map #(out->transition [k %] kc) v))
                              (for [[k v] (vc :in)]
                                (map #(in->transition [k %] kc) v)))))))
      [kc :start?] (true? (get vc :start?)))
     [kc :final?] (true? (get vc :final?)))
    [kc] dissoc :in)
   [kc] dissoc :out))


(defn all-front->back
  [struct]
  {:start-node (get-start-node struct)
   :nodes
   (loop [remaining (rest struct)
          [kc vc]   (first struct)
          new-struct {}]
     (if (empty? kc)
       new-struct
       (recur (rest remaining) (first remaining)
              (single-front->back kc vc new-struct))))})
