(defproject logic-puz "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot logic-puz.core
  :target-path "target/%s"
  :plugins [[cider/cider-nrepl "0.8.2"]]
  :profiles {:uberjar {:aot :all}})