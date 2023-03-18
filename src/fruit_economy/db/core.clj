(ns fruit-economy.db.core
  (:require [fruit-economy.db.protocol :as p]
            [fruit-economy.db.datascript-impl]
            [fruit-economy.db.datalevin-impl]))



(defn
  ^{:arglists '([query & inputs])
    :doc "Executes a datalog query. See [docs.datomic.com/on-prem/query.html](https://docs.datomic.com/on-prem/query.html).

          Usage:

          ```
          (q '[:find ?value
               :where [_ :likes ?value]]
             db)
          ; => #{[\"fries\"] [\"candy\"] [\"pie\"] [\"pizza\"]}
          ```"}
  q
  ([query db] (p/query db query))
  ([query db & inputs] (p/query db query inputs)))

(def ^{:arglists '([db index] [db index c1] [db index c1 c2] [db index c1 c2 c3] [db index c1 c2 c3 c4])
       :doc "Index lookup. Returns a sequence of datoms (lazy iterator over actual DB index) which components (e, a, v) match passed arguments.

   Datoms are sorted in index sort order. Possible `index` values are: `:eavt`, `:aevt`, `:avet`.

   Usage:

       ; find all datoms for entity id == 1 (any attrs and values)
       ; sort by attribute, then value
       (datoms db :eavt 1)
       ; => (#datascript/Datom [1 :friends 2]
       ;     #datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [1 :name \"Ivan\"])

       ; find all datoms for entity id == 1 and attribute == :likes (any values)
       ; sorted by value
       (datoms db :eavt 1 :likes)
       ; => (#datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"])

       ; find all datoms for entity id == 1, attribute == :likes and value == \"pizza\"
       (datoms db :eavt 1 :likes \"pizza\")
       ; => (#datascript/Datom [1 :likes \"pizza\"])

       ; find all datoms for attribute == :likes (any entity ids and values)
       ; sorted by entity id, then value
       (datoms db :aevt :likes)
       ; => (#datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       ; find all datoms that have attribute == `:likes` and value == `\"pizza\"` (any entity id)
       ; `:likes` must be a unique attr, reference or marked as `:db/index true`
       (datoms db :avet :likes \"pizza\")
       ; => (#datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       ; find all datoms sorted by entity id, then attribute, then value
       (datoms db :eavt) ; => (...)

   Useful patterns:

       ; get all values of :db.cardinality/many attribute
       (->> (datoms db :eavt eid attr) (map :v))

       ; lookup entity ids by attribute value
       (->> (datoms db :avet attr value) (map :e))

       ; find all entities with a specific attribute
       (->> (datoms db :aevt attr) (map :e))

       ; find “singleton” entity by its attr
       (->> (datoms db :aevt attr) first :e)

       ; find N entities with lowest attr value (e.g. 10 earliest posts)
       (->> (datoms db :avet attr) (take N))

       ; find N entities with highest attr value (e.g. 10 latest posts)
       (->> (datoms db :avet attr) (reverse) (take N))

   Gotchas:

   - Index lookup is usually more efficient than doing a query with a single clause.
   - Resulting iterator is calculated in constant time and small constant memory overhead.
   - Iterator supports efficient `first`, `next`, `reverse`, `seq` and is itself a sequence.
   - Will not return datoms that are not part of the index (e.g. attributes with no `:db/index` in schema when querying `:avet` index).
     - `:eavt` and `:aevt` contain all datoms.
     - `:avet` only contains datoms for references, `:db/unique` and `:db/index` attributes."}
  datoms
  d/datoms)

(def ^{:arglists '([db eid])
       :doc "Retrieves an entity by its id from database. Entities are lazy map-like structures to navigate DataScript database content.

             For `eid` pass entity id or lookup attr:

                 (entity db 1)
                 (entity db [:unique-attr :value])

             If entity does not exist, `nil` is returned:

                 (entity db 100500) ; => nil

             Creating an entity by id is very cheap, almost no-op, as attr access is on-demand:

                 (entity db 1) ; => {:db/id 1}

             Entity attributes can be lazily accessed through key lookups:

                 (:attr (entity db 1)) ; => :value
                 (get (entity db 1) :attr) ; => :value

             Cardinality many attributes are returned sequences:

                 (:attrs (entity db 1)) ; => [:v1 :v2 :v3]

             Reference attributes are returned as another entities:

                 (:ref (entity db 1)) ; => {:db/id 2}
                 (:ns/ref (entity db 1)) ; => {:db/id 2}

             References can be walked backwards by prepending `_` to name part of an attribute:

                 (:_ref (entity db 2)) ; => [{:db/id 1}]
                 (:ns/_ref (entity db 2)) ; => [{:db/id 1}]

             Reverse reference lookup returns sequence of entities unless attribute is marked as `:db/isComponent`:

                 (:_component-ref (entity db 2)) ; => {:db/id 1}

             Entity gotchas:

             - Entities print as map, but are not exactly maps (they have compatible get interface though).
             - Entities are effectively immutable “views” into a particular version of a database.
             - Entities retain reference to the whole database.
             - You can’t change database through entities, only read.
             - Creating an entity by id is very cheap, almost no-op (attributes are looked up on demand).
             - Comparing entities just compares their ids. Be careful when comparing entities taken from different dbs or from different versions of the same db.
             - Accessed entity attributes are cached on entity itself (except backward references).
             - When printing, only cached attributes (the ones you have accessed before) are printed. See [[touch]]."}
  entity
  d/entity)

(def ^{:arglists '([e])
       :doc "Forces all entity attributes to be eagerly fetched and cached. Only usable for debug output.

             Usage:

             ```
             (entity db 1) ; => {:db/id 1}
             (touch (entity db 1)) ; => {:db/id 1, :dislikes [:pie], :likes [:pizza]}
             ```"}
  touch
  d/touch)

(defn init-db []
  (d/empty-db {:land/history {:db/valueType :db.type/ref
                              :db/cardinality :db.cardinality/many}
               :land/resources {:db/valueType :db.type/ref
                                :db/cardinality :db.cardinality/many}
               :land/units {:db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}
               :land/civs {:db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/many}
               :civ/territory {:db/valueType :db.type/ref
                               :db/cardinality :db.cardinality/many}
               :civ/peeps {:db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/many}}))

(defn db-bulk-insert [db entities]
  (d/db-with db entities))
