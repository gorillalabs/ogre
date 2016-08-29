(ns clojurewerkz.ogre.core
  (:refer-clojure :exclude [filter and or range count iterate next map loop reverse group-by key shuffle repeat])
  (:require [potemkin :as po]
            [clojurewerkz.ogre.util :as util :refer (keywords-to-str-array typed-traversal ensure-traversal-source)]
            [clojurewerkz.ogre.filter :as filter]
            [clojurewerkz.ogre.map :as map]
            [clojurewerkz.ogre.traversal :as traversal]
            [clojurewerkz.ogre.branch :as branch]
            [clojurewerkz.ogre.side-effect :as side-effect]))

;; Define the traversal methods
(doseq [[direction short shortE name1] '((both <-> <E> both-vertices)
                                         (in   <-- <E- in-vertex)
                                         (out  --> -E> out-vertex))]
  (let [j1 (symbol (str "." direction))
        f1 (symbol (str direction "-edges"))
        j2 (symbol (str "." direction "E"))
        j3 (symbol (str "." direction "V"))]
    (eval `(do
             (defn ~direction
               ;; ~(str "Traverses edges along the "
               ;;       direction
               ;;       " direction and returns the vertices.")
               ([t#] (~direction t# []))
               ([t# labels#]
                (typed-traversal ~j1 (ensure-traversal-source t#) (keywords-to-str-array labels#))))
             (defn ~short
               [& args#]
               (apply ~direction args#))
             (defn ~f1
               ([t#] (~f1 t# []))
               ([t# labels#]
                (typed-traversal ~j2 t# (keywords-to-str-array labels#))))
             (defn ~shortE
               [& args#]
               (apply ~f1 args#))
             (defn ~name1
               [t#]
               (typed-traversal ~j3 t#))))))

(po/import-fn util/anon-traversal)
(defn __ [] (anon-traversal))

;; clojurewerkz.ogre.util
(po/import-fn util/as)
(po/import-macro util/query)
(po/import-macro util/subquery)

;; clojurewerkz.ogre.filter steps
(po/import-fn filter/coin)
(po/import-fn filter/cyclic-path)
(po/import-fn filter/dedup)
(po/import-fn filter/except)
(po/import-fn filter/filter)
(po/import-fn filter/has)
(po/import-fn filter/has-not)
(po/import-fn filter/interval)
(po/import-fn filter/limit)
(po/import-fn filter/range)
(po/import-fn filter/retain)
(po/import-fn filter/simple-path)
(po/import-macro filter/where)
(po/import-macro filter/or)

;; clojurewerkz.ogre.map steps
(po/import-fn map/back)
(po/import-fn map/id)
(po/import-fn map/fold)
(po/import-fn map/key)
(po/import-fn map/label)
(po/import-fn map/local)
(po/import-fn map/map)
(po/import-fn map/by)
(po/import-fn map/other-v)
(po/import-fn map/path)
(po/import-fn map/properties)
(po/import-fn map/order)
(po/import-fn map/order-by)
(po/import-fn map/select)
(po/import-fn map/select-only)
(po/import-fn map/shuffle)
(po/import-fn map/unfold)
(po/import-fn map/values)
(po/import-fn map/value-map)
(po/import-macro map/match)

;; clojurewerkz.ogre.traversal steps
(po/import-fn traversal/V)
(po/import-fn traversal/E)
(po/import-fn traversal/outV)
(po/import-fn traversal/outE)
(po/import-fn traversal/has-id)
(po/import-fn traversal/has-label)
(po/import-fn traversal/all-into-vecs!)
(po/import-fn traversal/all-into-sets!)
(po/import-fn traversal/all-into-maps!)
(po/import-fn traversal/count!)
(po/import-fn traversal/first-of!)
(po/import-fn traversal/first-into-vec!)
(po/import-fn traversal/first-into-set!)
(po/import-fn traversal/first-into-map!)
(po/import-fn traversal/into-lazy-seq!)
(po/import-fn traversal/into-list!)
(po/import-fn traversal/into-vec!)
(po/import-fn traversal/into-set!)
(po/import-fn traversal/iterate!)
(po/import-fn traversal/next!)
(po/import-macro traversal/repeat)
(po/import-fn traversal/emit)
(po/import-fn traversal/times)

;; clojurewerkz.ogre.side-effect steps
(po/import-fn side-effect/aggregate)
(po/import-fn side-effect/cap)
(po/import-fn side-effect/count)
(po/import-fn side-effect/is)
(po/import-fn side-effect/side-effect)
(po/import-fn side-effect/subgraph)
(po/import-fn side-effect/get-capped!)
(po/import-fn side-effect/get-grouped-by!)
(po/import-fn side-effect/get-group-count!)
(po/import-fn side-effect/group-by)
(po/import-fn side-effect/group-count)

;; clojurewerkz.ogre.branch steps
(po/import-macro branch/choose)
