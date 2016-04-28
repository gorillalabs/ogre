(ns clojurewerkz.ogre.filter
  (:refer-clojure :exclude [filter and or range])
  (:import
    (java.util Collection)
    (org.apache.tinkerpop.gremlin.process.traversal P Traversal Compare))
  (:require [clojurewerkz.ogre.util :refer (f-to-function f-to-predicate typed-traversal f-to-bipredicate anon-traversal f-to-compare)]))

(defn cyclic-path
  "The step analyzes the path of the traverser thus far and if there are any repeats, the traverser
  is filtered out over the traversal computation."
  [^Traversal t]
  (typed-traversal .cyclicPath t))

(defn dedup
  "Filters out repeated objects. A function can be supplied that provides the
  values that the traversal will consider when filtering."
  ([^Traversal t]
   (typed-traversal .dedup t))
  ([^Traversal t f]
   (typed-traversal #(.by (.dedup %) (f-to-function f)) t)))

(defn except
  "Filters out the given objects."
  [^Traversal t excepter]
  (cond
    (instance? String excepter) (typed-traversal .except t ^String excepter)
    (instance? Collection excepter) (typed-traversal .except t ^Collection excepter)
    :else (except t [excepter])))

(defn filter
  "Filters using a predicate that determines whether an object should pass."
  [^Traversal t f] (typed-traversal .filter t (f-to-predicate f)))

;TODO should be temporary/moved
(defn- string-or-keyword
  "Checks if the given value is either a string or keyword."
  [value]
  (clojure.core/or (string? value) (keyword? value)))

;TODO should be temporary/moved
(defn- cast-param
  "Value is either a T, String, or keyword. If it's a keyword, pass the name."
  [value]
  (if (keyword value)
    (name value)
    value))

;TODO this needs a cleanup, has step signatures have changed a lot
(defn has
  "Allows an element if it has the given property or it satisfies given predicate."
  ([^Traversal t k]
   (typed-traversal .has t (cast-param k)))
  ([^Traversal t k v-or-pred]
   (if (clojure.core/and (ifn? v-or-pred) (not (keyword? v-or-pred)))
     (has t k (fn [v _] (v-or-pred v)) :dummy)
     (typed-traversal .has t (cast-param k) v-or-pred)))
  ([^Traversal t k pred v]
   (if (clojure.core/and (string-or-keyword k) (string-or-keyword pred) (string-or-keyword v))
     (. t has (cast-param k) (cast-param pred) (cast-param v))
     (if-let [c (f-to-compare pred)]
       (typed-traversal .has t (cast-param k) ^Compare (c v))
       (typed-traversal .has t (cast-param k) (P. (f-to-bipredicate pred) v))))))

(defn has-not
  "Allows an element if it does not have the given property."
  ([^Traversal t k]
   (typed-traversal .hasNot t (name k)))
  ([^Traversal t k v-or-pred]
   (if (ifn? v-or-pred)
     (has t k (complement v-or-pred))
     (has t k not= v-or-pred)))
  ([^Traversal t k pred v]
   (has t k (complement pred) v)))

(defn interval
  "Allows elements to pass that have their property in the given start and end interval."
  [^Traversal t key ^Comparable start ^Comparable end]
  (typed-traversal .between t (name key) start end))

(defn limit
  "Limit the number of elements to pass through Traversal."
  [^Traversal t l] (typed-traversal .limit t l))

(defn coin
  "Allows elements to pass with the given probability."
  [^Traversal t probability] (typed-traversal .coin t probability))

(defn range
  "Allows elements to pass that are within the given range."
  [^Traversal t low high] (typed-traversal .range t low high))

(defn retain
  "Only allows the given objects to pass."
  [^Traversal t retainer]
  (cond
    (instance? String retainer) (typed-traversal .retain t ^String retainer)
    (instance? Collection retainer) (typed-traversal .retain t ^Collection retainer)
    :else (retain t [retainer])))

(defn simple-path
  "Allows an element if the current path has no repeated elements."
  [^Traversal t] (typed-traversal .simplePath t))

(defmacro where
  "Further constrain results from match with a binary predicate or traversal."
  ([^Traversal t pred a b]
   `(typed-traversal .where ~t (name ~a) ((f-to-compare ~pred) (name ~b))))
  ([^Traversal t constraint]
   `(typed-traversal .where ~t (-> (anon-traversal) ~constraint))))

(defmacro or [^Traversal t & filters]
  (let [filter-traversals (reduce #(conj %1 `(-> (anon-traversal) ~%2)) [] filters)]
    `(.or ~t (into-array Traversal ~filter-traversals))))







