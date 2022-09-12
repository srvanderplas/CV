# How to use Make on Windows:
# https://stackoverflow.com/questions/2532234/how-to-run-a-makefile-in-windows

.PHONY: default

# Use texfot if available. Produces less output.
TEXFOT_ARGS=--no-interactive --ignore 'Underfull.*' --ignore 'Package polytable Warning: Redefining column' --ignore 'Package hyperref Warning: Token not allowed in a PDF string' --ignore 'LaTeX Warning: .h. float specifier changed to .ht..'

TEXFOT=$(shell which texfot >/dev/null && echo texfot "${TEXFOT_ARGS}" || echo "")
# TEXFOT=

QUARTO = quarto render '$<'

default: SusanVanderplas-CV.pdf

# INFO: If the file repeatedly fails to compile, use:
# pdflatex -interaction nonstopmode -file-line-error paper.tex
%.pdf: %.tex
	latexmk --xelatex -g -pv -pdf $<

# latexmk -g -pdf -pdflatex="${CHRONIC} ${TEXFOT} pdflatex" $<
# ${CHRONIC} ${TEXFOT} bibtex
# ${TEXFOT} pdflatex $<
# ${TEXFOT} pdflatex $<

clean:
	rm -f *.pdf *.out *aux *bbl *blg *log *toc *bcf *run.xml *.ptb *.tod *.fls *.fdb_latexmk *.lof *out.ps *blg

#watch:
#	while true; do \
#	  make -B $(WATCHMAKE); \
#	  inotifywait --exclude '.*swx' --exclude '.*swp' --exclude '.*~' -qre close_write .; \
#	done
