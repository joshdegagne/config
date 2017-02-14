{:user {:aliases { "repl" ["do" ["clean"] ["repl"]]
                  "dumbrepl" ["with-profile" "repl" ["trampoline" "run" "-m" "clojure.main/main"]]
                  ;; linters
                  "kibit"     ["update-in" ":plugins" "conj" "[lein-kibit \"0.1.2\" :exclusions [org.clojure/clojure]]" "--" "kibit"]
                  "eastwood"  ["update-in" ":plugins" "conj" "[jonase/eastwood \"0.2.3\"]" "--" "eastwood"]
                  "slamhound" ["update-in" ":dependencies" "conj" "[slamhound \"1.5.5\"]" "--" ["run" "-m" "slam.hound"]]
                  "bikeshed"  ["update-in" ":plugins" "conj" "[lein-bikeshed \"0.4.0\"]" "--" "bikeshed"]
                  "yagni"     ["update-in" ":plugins" "conj" "[venantius/yagni \"0.1.4\"]" "--" "yagni"]
                  "cljfmt"    ["update-in" ":plugins" "conj" "[lein-cljfmt \"0.5.6\"]" "--" "cljfmt"]
                  ;; automated testing
                  "cloverage" ["update-in" ":plugins" "conj" "[lein-cloverage \"1.0.9\"]" "--" "cloverage"]
                  "test-out"  ["update-in" ":plugins" "conj" "[lein-test-out \"0.3.1\"]" "--" "test-out"]
                  ;; documentation
                  "codox"       ["update-in" ":plugins" "conj" "[codox \"0.10.1\"]" "--" "codox"]
                  "clojuredocs" ["update-in" ":plugins" "conj" "[lein-clojuredocs \"1.0.2\"]" "--" "clojuredocs"]
                  ;; package management
                  "ancient" ["update-in" ":plugins" "conj" "[lein-ancient \"0.6.10\" :exclusions [org.clojure/clojure]]" "--" "ancient"]
                  ;; application server
                  "immutant" ["update-in" ":plugins" "conj" "[lein-immutant \"2.1.0\"]" "--" "immutant"]}
        :plugins [[venantius/ultra "0.5.0"]] :ultra {:color-scheme :solarized_dark}}
 :repl {:repl-options {:timeout 12000000}
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
        :plugins [[cider/cider-nrepl "0.14.0-SNAPSHOT"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]
                  ;; bytecode inspection
                  [lein-nodisassemble "0.1.3"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [org.clojars.gjahad/debug-repl "0.3.3"]
                       [difform "1.1.2"]
                       [alembic "0.3.2"]
                       [im.chit/lucid.core.inject "1.2.0"]
                       [im.chit/lucid.mind "1.2.0"]
                       [io.aviso/pretty "0.1.31"]
                       ;; debugging
                       [spyscope "0.1.6"]
                       [philoskim/debux "0.2.1"]
                       [org.clojure/tools.trace "0.7.9"]
                       [org.clojure/tools.namespace "0.2.11"]
                       ;; benchmarking
                       [criterium "0.4.4"]
                       ;; visualization
                       [rhizome "0.2.7"]]
        :injections [(require '[clojure.java.io :as jio]
                              '[debux.core :as dx]
                              '[lucid.core.inject :as inject]
                              '[spyscope.core])
                     (inject/in [lucid.core.inject :refer [inject [in inject-in]]]
                                [clojure.pprint pprint]
                                [clojure.java.shell sh]
                                [debux.core dbg]
                                [alembic.still [distill pull] lein [load-project pull-project]]
                                [clojure.tools.namespace.repl refresh]
                                [clojure.repl doc source]
                                clojure.core
                                [lucid.mind .& .> .? .* .% .%> .>var .>ns])]}}
