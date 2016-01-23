CC=ghc

Main: Main.hs Parser.hs
	$(CC) Main.hs

Parser.hs:
	happy Parser.y

clean:
	rm *hi *o Main
