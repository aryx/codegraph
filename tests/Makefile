#TODO: c java js ml php
TESTDIRS=cmt

.PHONY: test
test:
	set -e; for i in $(TESTDIRS); do (cd $$i; $(MAKE) test); done
