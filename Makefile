
.PHONY: build dev-tools

build: reactjs-bindings/lib.js
	cabal configure --ghcjs
	cabal build

reactjs-bindings/lib.js: reactjs-bindings/lib.require.js
	cd reactjs-bindings; grunt

dev-tools:
	cd reactjs-bindings; npm install

