PACKAGE = $(shell sed -n 's/^name: *\(.*\)$$/\1/p' < graphwiz.cabal)
VERSION = $(shell sed -n 's/^version: *\(.*\)$$/\1/p' < graphwiz.cabal)
TARBALL = "dist-newstyle/sdist/$(PACKAGE)-$(VERSION).tar.gz"
DOCSTGZ = "documentation/$(PACKAGE)-$(VERSION)-docs.tar.gz"

define USAGE
Available commands:

    help       display this help
    build      builds all executables
    test       builds the project and run tests
    coverage   same as `make test`, but enables coverage
    candidate  builds and upload a candidate to hackage
endef

export USAGE


help:
	@echo "$$USAGE"

build:
	cabal build all

test:
	cabal test all

coverage:
	cabal test all --enable-coverage -f export-internals-for-coverage

candidate:
	cabal sdist
	cabal haddock --haddock-for-hackage --builddir documentation
	cabal upload $(TARBALL)
	cabal upload -d $(DOCSTGZ)


.PHONY: help build test coverage candidate
