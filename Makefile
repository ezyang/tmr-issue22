default: Author.pdf

%.tex: %.lhs force
	lhs2TeX --poly $< > $@

%.pdf: %.tex force
	pdflatex $<
	bibtex $(<:.tex=)
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.log *.aux *.toc *.out *.blg *.bbl *~ *-darcs-backup* *.tex *.ptb

bib : 

.PHONY : force

TMR.zip: Author.* Makefile tmr.*
	make clean
	zip TMR.zip Author.* Makefile tmr.*

export: TMR.zip
	scp TMR.zip byorgey@code.haskell.org:public_html/TMR/