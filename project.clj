(defproject io.dominic/lein-bikeshed "0.1-SNAPSHOT"
  :description (str "A Leiningen plugin designed to tell you your code is bad, "
                    "and that you should feel bad")
  :url "https://github.com/SevereOverfl0w/lein-bikeshed"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :eval-in :leiningen
  :dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]
                 [org.clojure/tools.analyzer.jvm "0.7.0"]
                 [org.clojure/tools.reader "1.0.0-beta4"]])
