all: Tetris

Tetris: Tetris.hs TetrisPieces.hs
	ghc --make Tetris.hs

clean:
	rm -rf *.o *.hi Tetris