(ns clojurewerkz.ogre.branch
  (:import (org.apache.tinkerpop.gremlin.process.traversal Traversal))
  (:require [clojurewerkz.ogre.util :refer (typed-traversal f-to-function f-to-predicate anon-traversal)]))

(defmacro choose
  "Select which branch to take based on predicate or jump map."
  ([^Traversal t k m]
   `(typed-traversal .choose ~t (f-to-function ~k)
                     ~(if (map? m)
                        (into {} (for [[k v] m]
                                   [k `(-> (anon-traversal) ~v)]))
                        m)))
  ([^Traversal t pred true-choice false-choice]
   (if (fn? pred)
     `(typed-traversal .choose ~t (f-to-predicate ~pred)
                       (-> (anon-traversal) ~true-choice)
                       (-> (anon-traversal) ~false-choice))
     `(typed-traversal .choose ~t (-> (anon-traversal)  ~pred)
                       (-> (anon-traversal) ~true-choice)
                       (-> (anon-traversal) ~false-choice)))))

;; jump
;; until
