#!/usr/bin/env make -f

SHELL 	= /bin/zsh
OUTDIR 	= ./dist-newstyle/out

clean:
	cabal clean

build:
	cabal build

install:
	cabal install --install-method=copy --installdir=$(OUTDIR)

test:
	cabal new-test --test-show-details=streaming

deploy: clean build install
