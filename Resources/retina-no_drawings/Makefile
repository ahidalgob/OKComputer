retina: Lexer Parser OurContextMonad ContextChecker
	ghc --make -w -rtsopts retina.hs

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

OurContextMonad: OurContextMonad.hs
	ghc --make -w OurContextMonad.hs

ContextChecker: ContextChecker.hs
	ghc --make -w ContextChecker.hs

AST: AST.hs
	ghc --make -w AST.hs

RunAST: RunAST.hs
	ghc --make -w RunAST.hs

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f Parser.hs
	-rm -f retina
