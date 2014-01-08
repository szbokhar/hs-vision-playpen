SHELL := /bin/bash

build-canny: dist
	cabal build canny 2>&1 | sed '/Loading package/d; /You are/d; /We will/d'

build-crop: dist
	cabal build crop 2>&1 | sed '/Loading package/d; /You are/d; /We will/d'

dist:
	cabal configure

clean: clean-gen
	cabal clean
	rm -f *.o *.hi new-* *.prof

clean-gen:
	rm -f images/edges-* images/back-*
