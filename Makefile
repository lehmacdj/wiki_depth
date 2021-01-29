all: wiki_depth

wiki_depth: wiki_depth.hs
	ghc -package http-conduit -package tagsoup --make wiki_depth

.PHONY: clean
clean:
	rm *.hi *.o wiki_depth
