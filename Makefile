SHELL := /bin/bash

build: dist
	cabal build canny 2>&1 | sed '/Loading package/d; /You are/d; /We will/d'

dist:
	cabal configure

clean:
	cabal clean
	rm *.o *.hi new-* *.prof
