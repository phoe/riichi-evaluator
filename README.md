mah-eval
========

Evaluates a mahjong hand.


## Usage

The program will read a line from standard input and attempt to parse it.
If it succeeds, a short summary of the hand is printed.
If it fails, "Error." is printed and the program will exit.


### Hand format

The basic format is a follows:

	<prevalent wind> <seat wind> (dora tiles) [(ura dora tiles)] tsumo|ron <tiles>

The final tile specified is taken to be the winning tile.
	
If ura dora tiles are listed, it is assumed the winning player declared riichi.
	
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
	1 x fanpai-chuu
	East pays 700, others pay 400.


The tiles in examples are ordered for clarity, but they can be in any order as long as
the winning tile is given last. The above hand could also be given as follows:

	east west (S) tsumo M4 S6 P2 (C C C) P3 M9 M7 P1 M8 M4 M4 S6

	1 han 40 fu
	1 x fanpai-chuu
	East pays 700, others pay 400.


Thirteen orphans, on a discard. Note that every yaku in the hand is listed, even ones that
don't increase the score.

	east south (M5) ron M1 M9 P1 P9 S1 S9 E E S W N B F C
	
	Yakuman (40 fu)
	1 x kokushi-musou
	1 x honroutou
	Discarder pays 32000.


A counted mangan (4 han 40 fu). Two dora hits.

	east west (S5) ron S6 (P3 P3 P3) (M4 M4 M4) (S3 S3 S3) (N N N) S6

	Kazoe-mangan (4 han 40 fu)
	1 x toitoi
	2 x dora
	Discarder pays 8000.


A counted yakuman with one regular dora and a closed kan of double ura dora.

	east west (P1 S2) (M5 M5) tsumo M3 M4 M5 P3 P4 P5 S4 S5 [M6 M6 M6 M6] M8 M8 S3

	Kazoe-yakuman (14 han 40 fu)
	1 x sanshoku-doujun
	1 x tanyao
	1 x menzen-tsumo
	1 x riichi
	1 x dora
	8 x ura-dora
	East pays 16000, others pay 8000.


The only double yakuman recognized by EMA Riichi rules, Big Four Winds.

	east west (P7) tsumo E E E S S S W W W N N N P1 P1
	Double-yakuman (60 fu)
	1 x daisuushii
	1 x shousuushii
	1 x suu-ankou
	1 x honroutou
	1 x honitsu
	1 x toitoi
	1 x fanpai-prevailing-wind
	1 x fanpai-seat-wind
	1 x menzen-tsumo
	East pays 32000, others pay 16000.


A hand without any yaku. Oops! The hand does have a dora hit, but that isn't counted as a yaku.

	east west (M5) ron M4 M5 M6 P4 P4 P4 S7 S7 S7 S1 S2 S3 W W
	No yaku.


The same hand with riichi declared.

	east west (M5) (C) ron M4 M5 M6 P4 P4 P4 S7 S7 S7 S1 S2 S3 W W

	2 han 50 fu
	1 x riichi
	1 x dora
	Discarder pays 3200.


