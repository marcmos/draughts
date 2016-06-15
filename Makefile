.PHONY: clean

hs:
	ghc -O2 Main.hs -o draughts

clean:
	rm -f draughts *.hi *.o
