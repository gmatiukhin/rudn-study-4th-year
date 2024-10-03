.PHONY: haskell tex watch

haskell:
	@mkdir -p ./assets/$(lab)/tex
	@stack run $(lab)-exe

tex: haskell
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pdf -quiet

watch:
	@mkdir -p ./assets/$(lab)/tex
	@echo "./app/$(lab).lhs" | entr -n -s "make haskell lab=$(lab) && make tex lab=$(lab)"

texwatch:
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pvc -pdf -quiet
