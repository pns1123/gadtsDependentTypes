# Makefile for building LaTeX presentation

OUTPUT_DIR = build
TEX_FILE = src/gadtsDependentTypesLecture.tex
PDF_FILE = $(OUTPUT_DIR)/gadtsDependentTypesLecture.pdf

# Default target
build: $(PDF_FILE)

# Rule for building the Lecture PDF file
$(PDF_FILE): $(TEX_FILE)
	mkdir -p $(OUTPUT_DIR)
	pdflatex -shell-escape -output-directory=$(OUTPUT_DIR) $<

# Clean rule
clean: 
	rm -rf $(OUTPUT_DIR)

# Phony targets
.PHONY: build clean
