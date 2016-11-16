all: wiki_depth

wiki_depth: wiki_depth.hs
	ghc --make wiki_depth

.PHONY: clean
clean:
	rm *.hi *.o wiki_depth
