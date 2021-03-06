(defproject clojure-learning "0.1.0-SNAPSHOT"
  :description "All my path in learning the most impressive language"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :plugins [[cider/cider-nrepl "0.9.1"]]
  :user {
         :jvm-opts ["-Dhttp.proxyHost=proxy" "-Dhttp.proxyPort=8080" "-Xss500m"]  
         :repositories {"maven" {:url "http://repo1.maven.org/maven2/"}}
         :javac-options {:destdir "classes/"}
         :java-source-path "src/main/java" ; location of Java source
         })
