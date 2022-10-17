 # Shortcuts

FORCE: ;

build: FORCE
	@cabal v2-build

clean: FORCE
	@cabal v2-clean

run: FORCE
	@cabal v2-run shell

sandbox: FORCE
	@cabal v2-run sandbox

test: FORCE
	@cabal --enable-tests v2-test

test-log: FORCE
	@less \"$(find . -name '*-test.log')\"
