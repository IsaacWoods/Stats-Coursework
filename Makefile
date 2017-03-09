.default: build

build:
	cabal install -j

run:
	.cabal-sandbox/bin/Stats
