all:
	ghc *.hs -o flp22-fun -Wall -O2

clean:
	rm *.o *.hi flp22-fun

run:
	./flp22-fun -o
