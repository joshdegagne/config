{:user {:aliases {"dumbrepl" ["with-profile" "repl" ["trampoline" "run" "-m" "clojure.main/main"]]
                  ;; formatting
                  "cljfmt" ["update-in" ":plugins" "conj" "[lein-cljfmt \"0.6.7\"]" "--" "cljfmt"]
                  ;; linters
                  "bikeshed" ["update-in" ":plugins" "conj" "[lein-bikeshed \"0.5.2\"]" "--" "bikeshed"]
                  "eastwood" ["update-in" ":plugins" "conj" "[jonase/eastwood \"0.3.11\"]" "--" "eastwood"]
                  "kibit" ["update-in" ":plugins" "conj" "[lein-kibit \"0.1.8\" :exclusions [org.clojure/clojure]]" "--" "kibit"]
                  "yagni" ["update-in" ":plugins" "conj" "[venantius/yagni \"0.1.7\"]" "--" "update-in" ":source-paths " "conj" "\"test\"" "--" "yagni"]
                  ;; testing
                  "cloverage" ["update-in" ":plugins" "conj" "[lein-cloverage \"1.1.2\"]" "--" "cloverage"]
                  ;; documentation
                  "codox" ["update-in" ":plugins" "conj" "[codox \"0.10.6\"]" "--" "codox"]
                  "clojuredocs" ["update-in" ":plugins" "conj" "[lein-clojuredocs \"1.0.2\"]" "--" "clojuredocs"]
                  ;; dependency management
                  "ancient" ["update-in" ":plugins" "conj" "[lein-ancient \"0.6.15\" :exclusions [org.clojure/clojure]]" "--" "ancient"]}}
 :repl {:repl-options {:timeout 12000000}
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
        :plugins [[nrepl/nrepl "0.7.0"]
                  [cider/cider-nrepl "0.25.1"]
                  [refactor-nrepl "2.5.0"]]
        :dependencies [;; debugging
                       [jsofra/data-scope "0.1.2"]
                       [philoskim/debux "0.6.5"]
                       [org.clojure/tools.trace "0.7.10"]
                       [org.clojure/tools.namespace "1.0.0"]
                       ;; visualization
                       [rhizome "0.2.9"]]}}