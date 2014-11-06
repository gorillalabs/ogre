(ns clojurewerkz.ogre.util
  (:import (com.tinkerpop.gremlin.process Traversal)
           (com.tinkerpop.gremlin.structure Compare Direction)
           (java.util.function Function Consumer Predicate)))

(defn as
  "Assigns a name to the previous step in a traversal."
  [^Traversal t label] (.as t label))

(defmacro query [xs & body]
  "Starts a query."
  `(-> ~xs ~@body))

(defmacro subquery
  "Starts a subquery."
  [& body]
  `(-> ~@body))

(defn ^Compare convert-symbol-to-compare [s]
  "Converts a symbolic comparison operator to a Gremlin structure comparison enumeration."
  (case s
    =    Compare/eq
    not= Compare/neq
    >=   Compare/gte
    >    Compare/gt
    <=   Compare/lte
    <    Compare/lt))

(defn ^"[Ljava.lang.String;" str-array [strs]
  "Converts a collection of strings to a java String array."
  (into-array String strs))

(defn keywords-to-str-array [strs]
  "Converts a collection of keywords to a java String array."
  (str-array (map name strs)))

(defn prop-map-to-array [m]
  "Converts a property map to a java Object array."
  (into-array Object
    (into []
      (flatten
        (map #(let [key (first %)
                    value (second %)]
            (vector (if (keyword? key) (name key) key) value)) m)))))

(defmulti convert-to-map "Converts objects to a map." class)

(defmethod convert-to-map java.util.HashMap
  [m]
  (into {} (for [[k v] m] [(keyword k) v])))

(defn ^Function f-to-function [f]
  "Converts a function to java.util.function.Function."
  (reify Function
    (apply [this arg] (f arg))))

(defn ^"[Ljava.util.function.Function;" fs-to-function-array
  "Converts a collection of functions to a java.util.function.Function array."
  [fs]
  (into-array Function (map f-to-function fs)))

(defn ^Consumer f-to-consumer [f]
  "Converts a function to java.util.function.Consumer."
  (reify Consumer
    (accept [this arg] (f arg))))

(defn ^Predicate f-to-predicate [f]
  "Converts a function to java.util.function.Predicate."
  (reify Predicate
    (test [this arg] (f arg))))

(defprotocol EdgeDirectionConversion
  (to-edge-direction [input] "Converts input to a Gremlin structure edge direction"))

(extend-protocol EdgeDirectionConversion
  clojure.lang.Named
  (to-edge-direction [input]
    (to-edge-direction (name input)))

  String
  (to-edge-direction [input]
    (case (.toLowerCase input)
      "in"    Direction/IN
      "out"   Direction/OUT
      "both"  Direction/BOTH))

  Direction
  (to-edge-direction [input]
    input))