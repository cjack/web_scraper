# makefile for web scraper

all: 
	R -f *.R

output:
	mkdir output
	cp *.txt *.csv output/
	zip output.zip output/*
clean:
	rm -rf output output.zip