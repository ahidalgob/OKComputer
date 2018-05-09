TestLex: Lexer
	ghc --make TestLex.hs

Lexer: Lexer.x
	alex Lexer.x

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f TestLex
