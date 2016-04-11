(ns clojurewerkz.ogre.element
  (:refer-clojure :exclude [keys vals assoc! dissoc! get])
  (:import (org.apache.tinkerpop.gremlin.structure Element Property)
           (org.apache.tinkerpop.gremlin.process.traversal Traverser))
  (:require [clojurewerkz.ogre.util :refer (keywords-to-str-array)]))

(defprotocol GetItemProperties
  "Returns properties of an item with the given key and with optional default value."
  (get [item key] [item key not-found]))

(extend-protocol GetItemProperties
  org.apache.tinkerpop.gremlin.structure.Element
  (get
    ([item key]
      (get item key nil))
    ([item key not-found]
      (let [^java.util.Iterator prop-iter (-> item (.properties (keywords-to-str-array [key])))
            prop (if (.hasNext prop-iter) (map #(.value ^Property %) (iterator-seq prop-iter)) (list not-found))]
        (if (= (count prop) 1) (first prop) prop))))

  Traverser
  (get
    ([item key]
      (get (.get item) key nil))
    ([item key not-found]
      (get (.get item) key not-found))))

(defn prop-pred
  "Returns a predicate to match the given property key and value."
  [key pred value item]
  (pred value (get item key)))

(defn keys
  "Returns the keys of an element."
  [^Element elem]
  (set (map keyword (.keys elem))))

(defn vals
  "Returns the values of an element."
  [^Element elem]
  (set (map #(-> elem (.property %) (.value)) (.keys elem))))

(defn id-of
  "Returns the id of an element."
  [^Element elem]
  (.id elem))

(defn label-of
  "Returns the label of the element."
  [^Element elem]
  (keyword (.label elem)))

(defn assoc!
  "Adds properties with the specified keys and values to an element."
  ([^Element elem key val]
   (.property elem (name key) val)
   elem)
  ([^Element elem key val & kvs]
   (assoc! elem key val)
   (doseq [kv (partition 2 kvs)]
     (.property elem (name (first kv)) (last kv)))
   elem))

(defn dissoc!
  "Removes properties with the specified keys from an element."
  [^Element elem & keys]
  (doseq [key keys] (.remove (.property elem (name key))))
  elem)

(defn clear!
  "Removes all properties from an element."
  [^Element elem]
  (apply dissoc! (cons elem (.keys elem))))

(defn merge! [^Element element & property-maps]
  (let [properties (flatten (seq (apply merge property-maps)))]
    (if (empty? properties)
      element
      (apply assoc! element properties))))

















