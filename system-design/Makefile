.PHONY: haskell tex watch

haskell:
	@stack run $(lab)-exe

tex: haskell
	@latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pdf -quiet

watch:
	@echo "./app/$(lab).lhs" | entr -n -s "make haskell lab=$(lab) && make tex lab=$(lab)"
	# @mkdir -p ./assets/$(lab)/tex
	# @latexmk ./app/$(lab).lhs -jobname=./assets/$(lab)/tex/$(lab) --shell-escape -pvc -pdf -quiet
