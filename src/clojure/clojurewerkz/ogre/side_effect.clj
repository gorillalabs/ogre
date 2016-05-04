(ns clojurewerkz.ogre.side-effect
  (:refer-clojure :exclude [group-by count])
  (:import (org.apache.tinkerpop.gremlin.process.traversal Traversal Traverser)
           (org.apache.tinkerpop.gremlin.process.traversal.dsl.graph GraphTraversal))
  (:require [clojurewerkz.ogre.traversal :as t]
            [clojurewerkz.ogre.util :refer (f-to-function f-to-consumer f-to-predicate typed-traversal)]))

;; addIn/Out*E

(defn aggregate
  "The aggregate step is used to aggregate all the objects at a particular point of traversal into a Collection. "
  ([^Traversal t ^String side-effect-key]
   (typed-traversal .aggregate t side-effect-key)))

(defn cap
  "Emits the value of the previous step and not the values that flow through it."
  ([^Traversal t] (typed-traversal .cap t))
  ([^Traversal t k] (typed-traversal .cap t (name k))))

(defn count
  "Count elements in the traversal."
  ([^Traversal t] (typed-traversal .count t)))

(defn is
  "Filter the E object if it not equal to the provided value"
  ([^Traversal t v] (typed-traversal .is t v)))


(defn get-capped!
  "Returns the value of the previous step."
  ([^Traversal t] (t/next! (cap t)))
  ([^Traversal t k] (t/next! (cap t k))))

(defn side-effect
  "Executes a side effect."
  [^Traversal t f]
  (typed-traversal .sideEffect t (f-to-consumer f)))

(defn group
  ([^GraphTraversal t] (typed-traversal .group t)))

(defn by
  ([^GraphTraversal t] (typed-traversal .by t)))

(defn group-by
  "Group objects by key function. Optionally transform each objects by value function and aggregate resultes by reduce function."
  ([^Traversal t keyfn]
   (group-by t keyfn #(.get ^Traverser %) identity))
  ([^Traversal t keyfn valfn]
   (group-by t keyfn valfn identity))
  ([^Traversal t keyfn valfn reducefn]
   (typed-traversal .group t (f-to-function keyfn) (f-to-function valfn) (f-to-function reducefn))))

(defn group-count
  "Returns the count of the objects.
Optionally grouped by key function or named by label."
  ([^Traversal t]
   (group-count t #(.get ^Traverser %)))
  ([^Traversal t keyfn-or-label]
   (if (ifn? keyfn-or-label)
     (typed-traversal .groupCount t (f-to-function keyfn-or-label))
     (typed-traversal .groupCount t (name keyfn-or-label))))
  ([^Traversal t label keyfn]
   (typed-traversal .groupCount t (name label) (f-to-function keyfn))))

(defn get-grouped-by!
  "Takes in a key function and processing function. Returns all of the processed objects grouped by the value of the key function."
  [^Traversal t key-func value-func]
  (->> (group-by t key-func value-func)
    t/into-vec!
    first
    (into {})
    (map (fn [[a b]] [a (vec b)]))
    (into {})))

(defn get-group-count!
  "Returns the count of the objects grouped by the key function."
  [^Traversal t key-func]
  (->> (group-count t key-func)
    t/into-vec!
    first
    (into {})))

;; inject
;; store

(defn subgraph
  "Produce an edge-induced subgraph from a traversal."
  ([^Traversal t edge-predicate] (typed-traversal .subgraph t (f-to-predicate edge-predicate)))
  ([^Traversal t side-effect-key edge-predicate] (typed-traversal .subgraph t side-effect-key (f-to-predicate edge-predicate)))
  ([^Traversal t edge-holder vertex-map edge-predicate] (typed-traversal .subgraph t edge-holder vertex-map (f-to-predicate edge-predicate)))
  ([^Traversal t side=effect-key edge-holder vertex-map edge-predicate] (typed-traversal .subgraph t side=effect-key edge-holder vertex-map (f-to-predicate edge-predicate))))

;; timelimit
;; tree
