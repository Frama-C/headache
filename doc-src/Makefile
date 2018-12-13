
all: manual.pdf manual.ps.gz manual.html manual.txt

clean:
	rm -f *.pdf *.ps *.ps.gz *.dvi *.aux *.log *.thm *.txt *.html

manual.dvi: manual.tex
	latex $^
	latex $^

manual.ps: manual.dvi
	dvips -t a4 -o $@ $^

manual.ps.gz: manual.ps
	gzip -c $^ > $@

manual.pdf: manual.dvi
	dvipdfm -p a4 -o $@ $^

manual.html: manual.dvi manual.tex
	hevea manual.tex

manual.txt: manual.dvi manual.tex
	hevea -text manual.tex
