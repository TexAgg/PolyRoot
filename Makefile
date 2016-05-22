TEXFILE=report

$(TEXFILE).pdf: $(TEXFILE).tex
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)
	bibtex $(TEXFILE)
	pdflatex $(TEXFILE)
	pdflatex $(TEXFILE)

clean:
	del *.aux *.blg *.out *.bbl *.log $(TEXFILE).run.xml $(TEXFILE)-blx.bib
	
view: $(TEXFILE).pdf
	$(TEXFILE).pdf &