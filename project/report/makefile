report.ps : report.dvi
		dvips report.dvi -o report.ps

report.dvi : report.tex
		latex report

clean : 
		rm report.aux report.log report.dvi report.blg report.bbl
	
bib	:	vision.bib report.tex
		latex report;bibtex report;latex report;latex report
