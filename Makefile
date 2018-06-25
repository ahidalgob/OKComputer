

OKC: Lexer Parser
	ghc -i./src:./src/LexerAndParser:./src/OKMonad --make OKC

Lexer: src/LexerAndParser/Lexer.x
	alex src/LexerAndParser/Lexer.x

Parser: src/LexerAndParser/Parser.y
	happy src/LexerAndParser/Parser.y

clean:
	# find . -type f -name "*.o" -exec rm "{}" +
	# find . -type f -name "*.hi" -exec rm "{}" +
	# find . -type f -name "*.info" -exec rm "{}" +
	# pretentious:
	find . -type f -regextype sed -regex ".*/*\.\(hi\|o\|info\)" -exec rm "{}" +
	find . -type f -name "Lexer.hs" -exec rm "{}" +
	find . -type f -name "Parser.hs" -exec rm "{}" +
	-rm -f OKC
