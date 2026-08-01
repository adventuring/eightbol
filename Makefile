# Skyline-Tool: EightBOL documentation builder
# Generates PDF, HTML, and Info files in Dist/

DOC_SRC = doc/EIGHTBOL.texi
DOC_DIR = ../doc
DIST_DIR = ../Dist

# Tools needed (install with: sudo apt install texinfo texlive-latex-base texi2html)
TEXI2PDF  = latex
INFO2HTML = texi2html

# Build rules
all: $(DIST_DIR)
	@echo "Generating documentation..."

$(DIST_DIR): $(DOC_SRC)
	@echo "Preparing output directory..."
	mkdir -p $(DIST_DIR)
	@echo "Processing $(DOC_SRC)..."
	$(INFO2HTML) $(DOC_DIR)/$(DOC_SRC) -o $(DIST_DIR)/$(notdir $(DOC_SRC:.texi=.html))
	@echo "Done."
	$(MAKE) clean

clean:
	rm -f $(DIST_DIR)/*
	rm -rf $(DIST_DIR)

.PHONY: all clean

