(ns fruit-economy.db.core)


;;; CRUD thing using reduce to take a list of changes
; [:insert]
; [:select]
; [:update]
; [:delete]
;;;   and update a denormalised map of paths
; [kind id {attrs values}]

(comment
  (let [ops [[:insert :civ 1 {:name "Civ 1" :peeps [:peep 2]}]
             [:insert :peep 2 {:name "Peep 1" :civ [:civ 1]}]]]
    (reduce
      (fn [db [op kind id attr-map]]
        (condp = op
          :insert (assoc-in db [kind id] attr-map)))

      {}
      ops)))
