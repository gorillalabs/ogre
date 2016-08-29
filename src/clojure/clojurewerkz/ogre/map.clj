(ns clojurewerkz.ogre.map
  (:refer-clojure :exclude [map key shuffle])
  (:import (org.apache.tinkerpop.gremlin.structure Element T)
           (org.apache.tinkerpop.gremlin.process.traversal.dsl.graph GraphTraversal)
           (org.apache.tinkerpop.gremlin.process.traversal Order Traverser Traversal)
           (org.apache.tinkerpop.gremlin.process.traversal.step.map MapStep))
  (:require [clojurewerkz.ogre.util :refer (f-to-function fs-to-function-array keywords-to-str-array keywords-to-str-list f-to-bifunction typed-traversal anon-traversal as)]))

(defn back
  "Goes back to the results of a named step."
  ([^Traversal t step-label] (typed-traversal .back t (name step-label))))

;; flatMap
;; fold(BiFunction)

(defn fold
  "Collects all objects up to the current step."
  ([^Traversal t] (typed-traversal .fold t)))

(defn id
  "Gets the unique identifier of the element."
  ([^GraphTraversal t] (.id t)))

;; identity

(defn key
  "Gets the key name of a Property."
  ([^GraphTraversal t] (.key t)))

(defn label
  "Gets the label of an element."
  ([^GraphTraversal t]
   (.label t)))

(defn local
  "Allows a traversal to operate on a single element within a stream."
  [^Traversal t local-t] (typed-traversal .local t local-t))

(defn map
  "Gets the property map of an element."
  ([^Traversal t f]
    (typed-traversal .map t (f-to-function f))))

(defmacro match
  "Pattern match traversals from current step onwards. Can introduce new labels."
  [^Traversal t & matches]
  `(typed-traversal .match ~t
                    (into-array ~(vec (for [[label m] (partition 2 matches)]
                                        `(-> (anon-traversal)
                                           (as ~label)
                                           ~m))))))



(def keyword->order {:decr Order/decr
                      :incr Order/incr})


;; todo: how best to resolve varargs overload to order
(defn order
  "Orders the items in the traversal according to the specified comparator
  or the default order if not specified."
  [^Traversal t] (.order t))

(defn cast-param
  "Value is either a T, String, or keyword. If it's a keyword, pass the name."
  [value]
  (if (keyword value)
    (name value)
    value))
#_(defn ^Function f-to-function [f]
  "Converts a function to java.util.function.Function."
  (reify Function
    (apply [this arg] (f arg))))

(defn by
  ([^GraphTraversal t]
   (.by t))
  ([^GraphTraversal t arg1]
   (cond
     (keyword? arg1)
     (.by t ^String (cast-param arg1))
     (instance? String arg1)
     (.by t ^String arg1)
     (instance? Order arg1)
     (.by t ^Order arg1)
     (instance? java.util.Comparator arg1)
     (.by t ^java.util.Comparator arg1)
     (instance? T arg1)
     (.by t ^T arg1)
     (instance? Traversal arg1)
     (.by t ^Traversal arg1)))
  ([^GraphTraversal t arg1 compar]
   (if (identical? :fn compar)
     (.by t (f-to-function arg1))
     (cond
       (keyword? arg1)
       (.by t ^String (cast-param arg1) ^java.util.Comparator compar)
       (instance? clojure.lang.IFn arg1)
       (.by t (f-to-function arg1) ^java.util.Comparator compar)
       (instance? T arg1)
       (.by t ^T arg1 ^java.util.Comparator compar)
       (instance? String arg1)
       (.by t ^String arg1 ^java.util.Comparator compar)
       (instance? Traversal arg1)
       (.by t ^Traversal arg1 ^java.util.Comparator compar)))))

 ; orderBy
(defn order-by
  ([^Traversal t direction]
   (typed-traversal .by (.order t) (direction keyword->order)))
  ([^Traversal t property direction]
   (typed-traversal .by (.order t) (name property) (direction keyword->order))))

(defn other-v
  "Gets the other vertex of an edge depending on which vertex a traversal started on."
  ([^Traversal t] (typed-traversal .otherV t)))

(defn path
  "Gets the path through the traversal up to the current step. If functions are provided
  they are applied round robin to each of the objects in the path."
  [^Traversal t]
    (typed-traversal .path t))

(defn properties
  "Gets the properties of an element."
  ([^Traversal t & keys]
    (typed-traversal .properties t (keywords-to-str-array keys))))

;; propertyMap

;; select overloads

(defn select
  "Get a list of named steps, with optional functions for post processing round robin style."
  ([^Traversal t]
    (.select t (into-array [])))
  ([^Traversal t & f]
    (typed-traversal .select t (fs-to-function-array f))))

(defn select-only
  "Select the named steps to emit."
  ([^GraphTraversal t col1]
   (typed-traversal .select t (name col1)))
  ([^GraphTraversal t col1 col2 & cols]
   (typed-traversal .select t (name col1) (name col2) (keywords-to-str-array cols))))

(defn shuffle
  "Collect all items in the traversal and randomize their order before emitting."
  ([^Traversal t] (typed-traversal .shuffle t)))

;; to

(defn unfold
  "Unroll all objects in the iterable at the current step."
  ([^Traversal t] (typed-traversal .unfold t)))

;; value
;; valueMap

(defn values
  "Gets the property values of an element."
  ([^Traversal t & keys]
    (typed-traversal .values t (keywords-to-str-array keys))))

(defn value-map
  [^GraphTraversal t & args]
  (if (clojure.core/and (clojure.core/not (empty? args)) (instance? Boolean (first args)))
    (.valueMap t (first args) (keywords-to-str-array (rest args))))
  (.valueMap t (keywords-to-str-array args)))

