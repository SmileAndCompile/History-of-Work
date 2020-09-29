# Makefile for zim.

### variables #################################################################

# The subdirectories requiring recursive make
SUBDIRS = syntax

# All documentation products
DOCS = Report.pdf

# All executatble products
BINS = zim

MODULES = zim.lhs \
          Zimm/Lexer.lhs \
          Zimm/Matching.lhs \
          Zimm/Token.lhs
          
### phony targets #############################################################

# Make everyting
.PHONY : all
all : subdirs_all doc bin
	
# Everything in the subdirectories
.PHONY : subdirs_all
subdirs_all : 
	for d in $(SUBDIRS);            \
	do                              \
	   $(MAKE) --directory=$$d all; \
	done

# make the documentation
.PHONY : doc
doc : subdirs_all $(DOCS)

# Make the executable
.PHONY : bin
bin : $(BINS)

# Run hlint and open the report
.PHONY : lint
lint : $(MODULES)
	! hlint . --report=hlint_report.html --quiet
	open hlint_report.html

# Clean up intermediate products -- leaves some for distribution
.PHONY : clean
clean :
	for d in $(SUBDIRS);              \
	do                                \
	   $(MAKE) --directory=$$d clean; \
	done
	rm -rf *.aux *.bbl *.blg *.brf *.log *.out *.toc  *.o *.hi report.html
	
# CLEAN up all products... back to the true sources
.PHONY : CLEAN
CLEAN : clean
	for d in $(SUBDIRS);              \
	do                                \
	   $(MAKE) --directory=$$d CLEAN; \
	done
	rm -rf *.pdf zim

### building ##################################################################

zim : zim.lhs Zimm/Lexer.lhs Zimm/Matching.lhs
	ghc --make zim.lhs
	
### typesetting ##################################################################

Report.pdf : Report.tex zim.bib $(MODULES)
	pdflatex Report.tex
	bibtex Report
	pdflatex Report.tex
	bibtex Report
	pdflatex Report.tex

### tests #####################################################################

t1 : zim
	./zim -run 20 realData/*/*.js
	open report.html
### typesetting ###############################################################
