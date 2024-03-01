# How to use Make on Windows:
# https://stackoverflow.com/questions/2532234/how-to-run-a-makefile-in-windows
.PHONY: clean clean-deps clean-bib all

# These are the R scripts which are used to make the components
RCODE = code/build_functions.R code/build_tex_files.R 
DATA = data/CV.xlsx

# These are essential prerequisites that have to be updated
COMPONENTS = tex-deps/check
# In order to have only one file dependency, we create a "check" file
# in the dependency folder that is updated when the files are rewritten

# -----------------------------------------------------------------------------
# This defines the files/targets you intend to make use of
all: CV.pdf

# -----------------------------------------------------------------------------
# This command creates the components and updates the tex-deps/check file
$(COMPONENTS): $(RCODE) $(DATA)
	Rscript "code/build_tex_files.R" \
	echo 'Component files built' 
	touch $(COMPONENTS)

	
# -----------------------------------------------------------------------------
# This command builds the PDF target (CV.pdf) from dependencies CV.tex, CV.bib,
# and the components above
%.pdf: %.tex data/%.bib $(COMPONENTS)
	latexmk -xelatex -g -pv -pdf $<

%.bib: 
	biber --tool -V $< 2>&1 $<.bibcheck

# -----------------------------------------------------------------------------
# This removes all dependencies/build files. Invoke with "make clean"
clean: clean-deps clean-bib
	rm -f *.pdf *.out *blg *log *toc *run.xml *.ptb *.tod *.fls *.fdb_latexmk *.lof *out.ps

clean-bib: 
	rm -f *aux *bbl *bcf 
	
clean-deps:
	rm -f tex-deps/*.tex tex-deps/check

