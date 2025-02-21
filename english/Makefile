%.docx: %.md
	pandoc -o $@ $< --citeproc --csl pandoc/apa.csl 

clean:
	rm *.docx
