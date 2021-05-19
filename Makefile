DESTDIR ?= $(HOME)/bin
default: all

all: 
		@echo Building tptp-utils ...
		sbt assembly
		mkdir bin -p
		cp tptp-utils-app/target/scala-2.13/tptp-utils-app-*.jar bin/.
		cat ./contrib/exec_dummy bin/tptp-utils-app-*.jar > bin/tptp-utils
		chmod +x bin/tptp-utils

install:
		install -m 0755 -d $(DESTDIR)
		install -m 0755 bin/tptp-utils $(DESTDIR)

clean:
		rm -rf bin/
		rm -rf target/
		rm -rf tptp-utils-app/target/
		rm -rf tptp-utils-runtime/target/
