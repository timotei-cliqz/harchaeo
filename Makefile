

clean:
	stack clean

dist-clean: clean
	@rm -vfr elm-stuff
	@rm -vfr .stack-work

setup:
	stack setup

.PHONY: build
build: setup
	stack build

backend: build
codegen: build

frontend: frontend/src/Generated/Api.elm index.html

frontend/src/Generated/Api.elm: codegen
	stack exec codegen

.PHONY: index.html
index.html:
	elm-make frontend/src/Main.elm

run: frontend backend
	stack exec backend -- --path ./archive.zip
