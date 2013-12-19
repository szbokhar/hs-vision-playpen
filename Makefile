SHELL := /bin/zsh

build: dist
	cabal build canny |& sed '/Loading package/d; /You are/d; /We will/d'

dist:
	cabal configure

clean:
	cabal clean
	rm '*.o *.hi new-* *.prof'
