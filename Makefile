all:
	ghc src/*.hs -o flp22-fun -Wall -O2

clean:
	rm src/*.o src/*.hi flp22-fun

run:
	./flp22-fun -o
