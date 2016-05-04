(ns clojurewerkz.ogre.filter.where-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojurewerkz.ogre.core :as q]
            [clojurewerkz.ogre.vertex :as v]
            [clojurewerkz.ogre.test-util :as u]))

(deftest where-test
  (testing "g.v().match(
           __.as('a').out('created').as('b'),
           __.as('b').in('created').as('c'))
           .where('a', neq, 'c')
           .select('a','c')
           .by('name')"
    (let [g (u/classic-tinkergraph)
          vs (q/query (q/V g)
                      (q/match :a (-> (q/out :created) (q/as :b))
                               :b (-> (q/in [:created]) (q/as :c)))
                      (q/where not= :a :c)
                      (q/select-only :a :c)
                      (q/by "name")
                      q/all-into-maps!) ]
      (is (= (count vs) 6))
      (is (every? (comp (fn [vs]
                          (= (count vs) (count (distinct vs))))
                        vals) vs))))

  (testing "g.V().match(
           __.as('a').out('created').as('b'),
           __.as('b').in('created').as('c'))
           .where(__.as('a').out('knows').as('c'))
           .select('a','c')
           .by('name')"
    (let [g (u/classic-tinkergraph)
          vs (q/query (q/V g)
                      (q/match :a (-> (q/out :created) (q/as :b))
                               :b (-> (q/in [:created]) (q/as :c)))
                      (q/where (-> (q/as :a) (q/out :knows) (q/as :c)))
                      (q/select-only :a :c)
                      (q/by "name")
                      q/first-into-map!
                      vals)]
      (is (every? #{"marko" "josh"} vs))))

  (testing "Jg.V ().has('age').where(__.in('knows').count().is(0))"
    (let  [g (u/classic-tinkergraph)
           vs  (q/query (q/V g)
                        (q/has :age)
                        (q/as :name)
                        (q/where (->
                                   (q/in :knows)
                                   (q/count)
                                   (q/is 0)
                                   ))
                        (q/select-only :name :name)
                        (q/by "name")
                        q/all-into-maps!
                        )]
      (is (= (count vs) 2))
      (is (some #(= (:name %) "marko") vs))
      (is (some #(= (:name %) "peter") vs)))))
