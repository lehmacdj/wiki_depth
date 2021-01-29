all: wiki_depth

PACKAGES = -package http-conduit -package tagsoup

wiki_depth: wiki_depth.hs
	ghc $(PACKAGES) --make wiki_depth

ghci: wiki_depth.hs
	ghci $(PACKAGES) wiki_depth.hs

clean:
	rm -rf *.hi *.o wiki_depth

.PHONY: clean ghci
