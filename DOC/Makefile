ALL = sl_201706_adoptFintech.pdf

%.pdf : %.tex
	latexmk -pdf $*.tex

all : $(ALL)

clean:
	rm -f *~ *aux *log *toc *bbl *blg *fls *fdb_latexmk *lot *lof *nav *out *snm *dvi *vrb

squeaky: clean
	rm -f $(ALL)

