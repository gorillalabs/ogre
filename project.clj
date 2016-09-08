(defproject gorillalabs/ogre "3.0.4.2"

  :description "Clojure dialect of the Gremlin graph processing language"
  :url "https://github.com/clojurewerkz/ogre"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [potemkin "0.4.3"]
                 [org.apache.tinkerpop/gremlin-core "3.1.1-incubating"]
                 [org.apache.tinkerpop/gremlin-test "3.1.1-incubating"  :scope "test" :exclusions [org.slf4j/slf4j-api]]
                 [org.apache.tinkerpop/tinkergraph-gremlin "3.1.1-incubating"  :scope "test" :exclusions [org.slf4j/slf4j-api]]]
  :source-paths ["src/clojure"]
  :profiles {:dev    { :global-vars {*assert* true}
                      :dependencies [[clojurewerkz/support "1.1.0" :exclusions [org.clojure/clojure]]
                                     [commons-io/commons-io "2.4"]]}
             :1.5    {:dependencies [[org.clojure/clojure "1.8.0"]]}}
  :aliases {"all" ["with-profile" "dev:dev,1.5:dev,1.7"]}
  :repositories {"sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"
                             :snapshots false
                             :releases {:checksum :fail :update :always}}
                 "sonatype-snapshots" {:url "http://oss.sonatype.org/content/repositories/snapshots"
                                       :snapshots true
                                       :releases {:checksum :fail :update :always}}}
  :plugins [[lein-junit "1.1.8"]]
  :java-source-paths ["test/java"]
  :junit ["test/java"]
  :test-paths ["test/clojure"]
  :global-vars {*warn-on-reflection* true
                *assert* false})
