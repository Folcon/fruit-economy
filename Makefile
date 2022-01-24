.PHONY: dev prod

dev:
	lein with-profile +macos,+dev trampoline run

prod: prod-mac # prod-win prod-linux ## Others can only be built on respective systems, so test later
	echo "DONE"
prod-mac:
	export JAVA_HOME=`/usr/libexec/java_home` && \
	cd releases && \
	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
	--input ../target --main-jar fruit-economy-0.1.0-SNAPSHOT-standalone.jar \
	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
	--java-options -XstartOnFirstThread
prod-win:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
    	--input ../target --main-jar fruit-economy-0.1.0-SNAPSHOT-standalone.jar \
    	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
    	--type exe
prod-linux:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
    	--input ../target --main-jar fruit-economy-0.1.0-SNAPSHOT-standalone.jar \
    	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
    	--type pkg --linux-shortcut
