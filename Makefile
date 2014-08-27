
.PHONY: build

reactjs-bindings/lib.js: reactjs-bindings/lib.require.js
	cd reactjs-bindings; grunt

build: reactjs-bindings/lib.js
	cabal build
