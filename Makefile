

OKC: Lexer Parser
	ghc -i./src:./src/LexerAndParser:./src/OKMonad --make OKC

Lexer: src/LexerAndParser/Lexer.x
	alex src/LexerAndParser/Lexer.x

Parser: src/LexerAndParser/Parser.y
	happy src/LexerAndParser/Parser.y

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f LexerAndParser/*.hi
	-rm -f LexerAndParser/*.o
	-rm -f LexerAndParser/Lexer.hs
	-rm -f LexerAndParser/Parser.hs
	-rm -f LexerAndParser/*.info
	-rm -f OKC
