# makefile for web scraper

all: 
	r -f *.R

output:
	mkdir output
	cp *.txt *.csv output/
	zip output.zip output/*
clean:
	rm -rf output output.zip