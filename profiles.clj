{:user {:aliases {;; time savers
                  "repl" ["do" ["clean"] ["repl"]]
                  "dumbrepl" ["with-profile" "repl" ["trampoline" "run" "-m" "clojure.main/main"]]
                  ;; linters
                  "kibit"     ["update-in" ":plugins"
                               "conj" "[lein-kibit \"0.1.2\"]"
                               "--" "kibit"]
                  "eastwood"  ["update-in" ":plugins"
                               "conj" "[jonase/eastwood \"0.2.3\"]"
                               "--" "eastwood"]
                  "slamhound" ["update-in" ":dependencies"
                               "conj" "[slamhound \"1.5.5\"]"
                               "--" ["run" "-m" "slam.hound"]]
                  "bikeshed"  ["update-in" ":plugins"
                               "conj" "[lein-bikeshed \"0.3.0\"]"
                               "--" "bikeshed"]
                  "yagni"     ["update-in" ":plugins"
                               "conj" "[venantius/yagni \"0.1.4\"]"
                               "--" "yagni"]
                  "cljfmt"    ["update-in" ":plugins"
                               "conj" "[lein-cljfmt \"0.5.3\"]"
                               "--" "cljfmt"]
                  ;; automated testing
                  "cloverage" ["update-in" ":plugins"
                               "conj" "[lein-cloverage \"1.0.6\"]"
                               "--" "cloverage"]
                  "test-out"  ["update-in" ":plugins"
                               "conj" "[lein-test-out \"0.3.1\"]"
                               "--" "test-out"]
                  ;; documentation
                  "codox"       ["update-in" ":plugins"
                                 "conj" "[codox \"0.9.5\"]"
                                 "--" "codox"]
                  "clojuredocs" ["update-in" ":plugins"
                                 "conj" "[lein-clojuredocs \"1.0.2\"]"
                                 "--" "clojuredocs"]
                  ;; package management
                  "ancient" ["update-in" ":plugins"
                             "conj" "[lein-ancient \"0.6.8\"]"
                             "--" "ancient"]
                  ;; application server
                  "immutant" ["update-in" ":plugins"
                              "conj" "[lein-immutant \"2.1.0\"]"
                              "--" "immutant"]}
        :plugins [[venantius/ultra "0.3.4"]]
        :ultra {:color-scheme :solarized_dark}}
 :repl {:repl-options {:timeout 12000000}
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
        :plugins [;; ide
                  [cider/cider-nrepl "0.13.0-SNAPSHOT"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]
                  ;; bytecode inspection
                  [lein-nodisassemble "0.1.3"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [org.clojars.gjahad/debug-repl "0.3.3"]
                       [org.clojure/tools.trace "0.7.8"]
                       [org.clojure/tools.namespace "0.2.10"]
                       ;; benchmarking
                       [criterium "0.4.3"]
                       ;; repl debugging
                       [spyscope "0.1.5"]
                       [philoskim/debux "0.2.0"]
                       [dire "0.5.4"]
                       [com.rpl/specter "0.9.3"]]
        :injections [(use 'debux.core)
                     (use 'clojure.java.io)
                     (require 'spyscope.core)
                     (require 'dire.core)
                     (require 'com.rpl.specter)]}}
