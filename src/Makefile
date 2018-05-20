OKC: Lexer Parser
	ghc -i./LexerAndParser --make OKC

Lexer: LexerAndParser/Lexer.x
	alex LexerAndParser/Lexer.x

Parser: LexerAndParser/Parser.y
	happy LexerAndParser/Parser.y

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f LexerAndParser/*.hi
	-rm -f LexerAndParser/*.o
	-rm -f LexerAndParser/Lexer.hs
	-rm -f LexerAndParser/Parser.hs
	-rm -f LexerAndParser/*.info
	-rm -f OKC
