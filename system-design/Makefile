.PHONY: haskell tex watch texwatch

all: mkdir haskell tex

mkdir:
	@mkdir -p assets/lab{1,2,3,4}/{tex,images}

haskell: mkdir
	@stack run $(lab)-exe

tex: mkdir
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pdf

watch:
	@echo "./app/$(lab).lhs" | entr -n -s "make all lab=$(lab)"

texwatch:
	@echo "./app/$(lab).lhs" | entr -n -s "make tex lab=$(lab)"
