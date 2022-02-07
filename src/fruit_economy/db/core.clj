(ns fruit-economy.db.core)


;;; CRUD thing using reduce to take a list of changes
; [:insert]
; [:select]
; [:update]
; [:delete]
;;;   and update a denormalised map of paths
; [kind id {attrs values}]

(defn db-insert [db [_op kind id attr-map]]
  (let [refs (into [] (remove (fn [[_k v]] (not (vector? v)))) attr-map)]
    (-> db
      (assoc-in [:data kind id] attr-map)
      (as-> $
        (reduce
          (fn [db [foreign-key [k v]]]
            (let [ref [kind id foreign-key]]
              (update-in db [:refs k v] (fnil conj #{}) ref)))
          $
          refs)))))

(defn db-update [db [_op kind id attr f & args]]
  (apply update-in db [:data kind id attr] f args))

(defn db-delete [db [_op kind id]]
  (let [refs (get-in db [:refs kind id])]
    (cond-> (update-in db [:data kind] dissoc id)
      refs
      (as-> $
        (reduce
          (fn [db [kind id foreign-key]]
            (update-in db [:data kind id] dissoc foreign-key))
          $
          refs)))))


(comment
  (let [ops [[:insert :civ 1 {:name "Civ 1" :peeps [:peep 2] :power 10}]
             [:insert :peep 2 {:name "Peep 1" :civ [:civ 1]}]
             [:update :civ 1 :power inc]
             [:delete :peep 2]]]
    (reduce
      (fn [db op]
        (condp = (first op)
          :insert (db-insert db op)
          :update (db-update db op)
          :delete (db-delete db op)))
      {}
      ops)))
