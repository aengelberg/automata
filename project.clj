(defproject automata "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [loco "0.3.1-SNAPSHOT"]
                 [rhizome "0.2.5"]]
  :jvm-opts ^:replace ["-server" "-Xmx4g"]
  :plugins [[refactor-nrepl "1.1.0"]
            [cider/cider-nrepl "0.9.1"]])
