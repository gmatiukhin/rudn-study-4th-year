.PHONY: haskell tex watch

haskell:
	@stack run $(lab)-exe

tex: haskell
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pdf

watch:
	@echo "./app/$(lab).lhs" | entr -n -s "make haskell lab=$(lab) && make tex lab=$(lab)"

texwatch:
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pvc -pdf
