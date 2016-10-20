TEXFILE=report

$(TEXFILE).pdf: $(TEXFILE).tex sources.bib
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)
	bibtex $(TEXFILE)
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)

.PHONY: clean
clean:
	rm -f *.aux *.blg *.out *.bbl *.log $(TEXFILE).run.xml $(TEXFILE)-blx.bib $(TEXFILE).synctex.gz
	
.PHONY: view
view: $(TEXFILE).pdf
	evince $(TEXFILE).pdf