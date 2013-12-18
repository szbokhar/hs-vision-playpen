build: dist
	cabal build canny

build-prof: dist
	cabal build canny-prof

dist:
	cabal configure

clean:
	cabal clean
	rm *.o *.hi
	rm new-* *.prof
