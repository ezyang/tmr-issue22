issue = Issue22

#lhssources = 
texsources = Editorial.tex gadt.tex article.tex monoids.tex

default: $(issue).pdf

$(issue).tex : $(issue).lhs $(texsources) $(lhssources)
	lhs2TeX $(issue).lhs > $(issue).tex

%.pdf: %.tex force
	pdflatex $<

%.tex: %.lhs
	lhs2TeX $< -o $@

clean:
	rm -f *.log *.aux *.toc *.out *.blg *.bbl *.ptb *~
	rm -f $(issue).tex

# put .bib files here
bib :
	bibtex gadt
	bibtex article

final : $(issue).pdf bib
	pdflatex $(issue).tex
	pdflatex $(issue).tex
	pdflatex $(issue).tex

.PHONY : force
