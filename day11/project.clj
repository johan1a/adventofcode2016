(defproject day11 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot day11.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
