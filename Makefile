

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

frontend: index.html frontend/src/Generated/Api.elm

frontend/src/Generated/Api.elm: codegen
	stack exec codegen

index.html:
	elm-make frontend/src/Main.elm

run: frontend backend
	stack exec backend -- ./archive
