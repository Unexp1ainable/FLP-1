all:
	ghc *.hs -o flp22-fun -Wall

clean:
	rm *.o *.hi flp22-fun