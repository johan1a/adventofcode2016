(defproject day17 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot day17.core
  :target-path "target/%s"
  :jvm-opts ["-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-Xmx2G"]
  :profiles {:uberjar {:aot :all}})
