mah-eval
========

Evaluates a mahjong hand.

## Usage

The program will read a line from standard input and attempt to parse it. If it fails to do so, "Error." is printed and the program will exit. If it succeeds, a short summary of the hand is printed.

### Hand format

The basic format is a follows:
	<prevalent wind> <seat wind> (dora tiles) [(ura dora tiles)] tsumo|ron <tiles>

The final tile specified is taken to be the winning tile.
	
If ura dora tiles are listed, it is assumed riichi was declared.
	
Open sets are specified as follows:
	(C C C)        ; Open pon of red dragons
	(P1 P2 P3)     ; Open chi, 1-2-3 of circles

Closed kans work similarly:
	[F F F F]      ; Closed kan of green dragons
	
Note that aside from kans, there is no need to specify closed sets.

### Examples

East round, west wins by self-draw. Open hand with a pon of dragons as the only yaku.
	east west (S) tsumo S6 P1 P2 P3 M7 M8 M9 M4 M4 M4 (C C C) S6
	1 han 40 fu
	  fanpai-chuu 
	  East pays 700, others pay 400.

The tiles in examples are ordered for clarity, but they can be in any order as long as the winning tile is given last. The above hand could also be given as follows:
	east west (S) tsumo M4 S6 P2 (C C C) P3 M9 M7 P1 M8 M4 M4 S6
	1 han 40 fu
	  fanpai-chuu 
	  East pays 700, others pay 400.

