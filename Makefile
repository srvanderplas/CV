# How to use Make on Windows:
# https://stackoverflow.com/questions/2532234/how-to-run-a-makefile-in-windows
.PHONY: default

default: SusanVanderplas-CV.pdf

COMPONENTS = $(wildcard _*.tex)

$(COMPONENTS): build_functions.R build_tex_files.R
	/usr/bin/Rscript "build_tex_files.R"
	echo 'Component files built'


%.pdf: %.tex $(COMPONENTS) %.bib
	/home/susan/.TinyTeX/bin/x86_64-linux/latexmk -xelatex -g -pv -pdf $<


clean:
	rm -f *.pdf *.out *aux *bbl *blg *log *toc *bcf *run.xml *.ptb *.tod *.fls *.fdb_latexmk *.lof *out.ps *blg
