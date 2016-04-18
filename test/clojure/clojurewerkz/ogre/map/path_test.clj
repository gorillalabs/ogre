(ns clojurewerkz.ogre.map.path-test
  (:import (org.apache.tinkerpop.gremlin.process.traversal Path))
  (:require [clojure.test :refer [deftest testing is]]
            [clojurewerkz.ogre.core :as q]
            [clojurewerkz.ogre.vertex :as v]
            [clojurewerkz.ogre.test-util :as u]))

(deftest test-path-step
  (testing "g.v(1).values('name').path().next()"
    (let [g (u/classic-tinkergraph)
          path (q/query (q/V g (int 1))
                        (q/values :name)
                        q/path
                        q/next!)
          path (.objects ^Path path)]
      (is (= "marko" (v/get (first path) :name)))
      (is (= "marko" (second path))))))
