build: dist
	cabal build

dist:
	cabal configure

clean:
	cabal clean
	rm new-*
