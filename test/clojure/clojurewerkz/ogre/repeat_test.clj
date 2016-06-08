(ns clojurewerkz.ogre.repeat-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojurewerkz.ogre.core :as q]
            [clojurewerkz.ogre.test-util :as u])
  )


(deftest repeat-test
  (testing "g.V().repeat(out()).times(2)"
    (let [g (u/classic-tinkergraph)
          vs (q/query (q/V g)
                      (q/repeat (q/out))
                      (q/times 2)
                      q/count!
                      )]
      (is (= vs 2))
      ))
)
