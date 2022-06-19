(defproject fruit-economy "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.specs.alpha "0.2.62"]
                 [org.clojure/spec.alpha "0.3.218"]
                 ;[org.projectlombok/lombok "1.18.22" :scope "provided"]
                 ;[org.jetbrains/annotations "20.1.0"]
                 [environ "1.2.0"]
                 [io.github.humbleui/types "0.1.2" :classifier "clojure"]
                 [io.github.humbleui/jwm "0.4.3" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-shared "0.102.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-windows "0.102.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-linux "0.102.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-macos-x64 "0.102.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-macos-arm64 "0.102.0" :exclusions [io.github.humbleui/types]]
                 [humbleui "acead0cfec02660e2ce8885bc40d23fdd118bd90"]
                 [datascript "1.3.13"]
                 [denistakeda/posh "0.5.9"]

                 [net.mikera/clisk "0.11.0"]
                 [ubergraph "0.8.2"]
                 [hiccup "1.0.5"]
                 [dali "1.0.2"]
                 [guru.nidi/graphviz-java "0.18.1"]
                 [com.squidpony/squidlib-util "3.0.4"]
                 [com.taoensso/timbre "5.2.1"]]
  :java-source-paths ["src/java" "test/java"]
  :plugins [[reifyhealth/lein-git-down "0.4.1"]
            [lein-environ "1.2.0"]]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {humbleui {:coordinates HumbleUI/HumbleUI}}
  :middleware [lein-git-down.plugin/inject-properties]
  :main fruit-economy.core
  :uberjar-name "fruit-economy-standalone.jar"
  :env {:game-version :project/version}
  :profiles {:macos {:jvm-opts  ["-XstartOnFirstThread"]}
             :dev {:jvm-opts  ["-Djdk.attach.allowAttachSelf" "-XX:+UnlockDiagnosticVMOptions" "-XX:+DebugNonSafepoints"]
                   :source-paths ["dev"]
                   :env {:debug? "true"}
                   :dependencies  [[nrepl/nrepl "0.9.0"]
                                   [com.clojure-goes-fast/clj-async-profiler "1.0.0-alpha1"]]
                   :main-opts   ["-m" "user" "--interactive"]}}
  :repl-options {:init-ns fruit-economy.core})
