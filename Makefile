TEXFILE=report

$(TEXFILE).pdf: $(TEXFILE).tex
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)
	bibtex $(TEXFILE)
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)

.PHONY: clean
clean:
	del *.aux *.blg *.out *.bbl *.log $(TEXFILE).run.xml $(TEXFILE)-blx.bib $(TEXFILE).synctex.gz
	
.PHONY: view
view: $(TEXFILE).pdf
	$(TEXFILE).pdf &