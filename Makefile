
.PHONY: build dev-tools

build: reactjs-bindings/lib.js
	cabal build

reactjs-bindings/lib.js: reactjs-bindings/lib.require.js reactjs-bindings/invariant.js
	cd reactjs-bindings; grunt

dev-tools:
	cd reactjs-bindings; npm install
	rm reactjs-bindings/lib.js

