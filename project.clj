(defproject paip "0.1.0-SNAPSHOT"
  :description "PAIP implementation of Search tools in clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot paip.core
  :target-path "target/%s"
  :plugins [[cider/cider-nrepl "0.15.0-SNAPSHOT"]]
  :repl-options {:init (set! *print-length* 50)}
  :profiles {:uberjar {:aot :all}
             :repl {:plugins [[cider/cider-nrepl "0.15.0-SNAPSHOT"]]}
             :dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.trace "0.7.9"]]}})
