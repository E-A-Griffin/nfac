(defproject nfac "0.1.0-SNAPSHOT"
  :description "Graphical application for generating NFAs and testing strings
               for membership in NFAs"
  :url "https://github.com/E-A-Griffin/nfac"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.5.527"]
                 [cider/cider-nrepl "0.28.5"]
                 [quil "4.0.0-SNAPSHOT"]
                 [robot "0.2.1-SNAPSHOT"]
                 [org.clojure/spec.alpha "0.3.218"]])
