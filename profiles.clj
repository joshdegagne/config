{:user {:aliases {"dumbrepl" ["with-profile" "repl" ["trampoline" "run" "-m" "clojure.main/main"]]
                  ;; formatting
                  "cljfmt" ["update-in" ":plugins" "conj" "[lein-cljfmt \"0.6.4\"]" "--" "cljfmt"]
                  ;; linters
                  "bikeshed"  ["update-in" ":plugins" "conj" "[lein-bikeshed \"0.5.2\"]" "--" "bikeshed"]
                  "eastwood"  ["update-in" ":plugins" "conj" "[jonase/eastwood \"0.3.5\"]" "--" "eastwood"]
                  "kibit"     ["update-in" ":plugins" "conj" "[lein-kibit \"0.1.6\" :exclusions [org.clojure/clojure]]" "--" "kibit"]
                  "yagni"     ["update-in" ":plugins" "conj" "[venantius/yagni \"0.1.7\"]" "--"
                               "update-in" ":source-paths " "conj" "\"test\"" "--"
                               "yagni"]
                  ;; testing
                  "cloverage" ["update-in" ":plugins" "conj" "[lein-cloverage \"1.1.1\"]" "--" "cloverage"]
                  ;; documentation
                  "codox"       ["update-in" ":plugins" "conj" "[codox \"0.10.6\"]" "--" "codox"]
                  "clojuredocs" ["update-in" ":plugins" "conj" "[lein-clojuredocs \"1.0.2\"]" "--" "clojuredocs"]
                  ;; dependency management
                  "ancient" ["update-in" ":plugins" "conj" "[lein-ancient \"0.6.15\" :exclusions [org.clojure/clojure]]" "--" "ancient"]}}
 :repl {:repl-options {:timeout 12000000}
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
        :plugins [[cider/cider-nrepl "0.21.1"]
                  [refactor-nrepl "2.4.0"]]
        :dependencies [;; debugging
                       [jsofra/data-scope "0.1.2"]
                       [philoskim/debux "0.2.1"]
                       [org.clojure/tools.trace "0.7.9"]
                       [org.clojure/tools.namespace "0.2.11"]
                       ;; visualization
                       [rhizome "0.2.7"]]}}
