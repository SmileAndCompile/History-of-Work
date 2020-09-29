### variables #################################################################

# All the ebnf files
ALLEBNFS = $(wildcard *.ebnf)

# All the ebnf base names
ALLBASES = $(basename $(ALLEBNFS))

# All the .eps file names
ALLEPSS = $(addsuffix .eps, $(ALLBASES))

# the command to convert eps to pdf
EPSTOPDF = epstopdf

# All the .pdf file names
ALLPDFS = $(addsuffix .pdf, $(ALLBASES))

# All the .tex file names
ALLTEXS = $(addsuffix .tex, $(ALLBASES))

# the syntrax command 
SYNTRAX = ~/bin/syntrax +check +fill

### rules #####################################################################

# convert .eps to .pdf
%.pdf : %.eps
	$(EPSTOPDF) $<

### phony targets #############################################################

# Everything
.PHONY : all
all : $(ALLEPSS) $(ALLPDFS)
	
# Clean up intermediate products
.PHONY : clean
clean :
	rm -r -f $(ALLEPSS) 

# Clean up ALL products
.PHONY : CLEAN
CLEAN : clean
	rm -r -f $(ALLPDFS)
	
### apply syntrax #############################################################

$(ALLEPSS) : $(ALLEBNFS)
	$(SYNTRAX) $(ALLEBNFS)
