DESTDIR ?= $(HOME)/bin
default: all

all: 
		@echo Building tptp-utils ...
		sbt +assembly
		mkdir bin -p
		cp tptp-utils-app/target/scala-3.*/tptp-utils-app-*.jar bin/tptp-utils.jar
		cat ./contrib/exec_dummy bin/tptp-utils.jar > bin/tptp-utils
		chmod +x bin/tptp-utils

install:
		install -m 0755 -d $(DESTDIR)
		install -m 0755 bin/tptp-utils $(DESTDIR)

clean:
		rm -rf bin/
		rm -rf target/
		rm -rf tptp-utils-app/target/
		rm -rf tptp-utils-runtime/target/

native:
		@echo Creating native image with graalvm
		sbt nativeImage
		mv tptp-utils-app/target/native-image/tptp-utils-app bin/tptp-utils-native
