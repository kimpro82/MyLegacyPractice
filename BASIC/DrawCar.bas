CLS

SCREEN 12       '640 x 480 / 16 colors
wid% = 640      'Can I get these parameters automatically?
hei% = 480

'Border
LINE (10, 10)-(wid% - 10, 10), 15, B
LINE (10, hei% - 80)-(wid% - 10, hei% - 80), 15, B
LINE (10, 10)-(10, hei% - 80), 15, B
LINE (wid% - 10, 10)-(wid% - 10, hei% - 80), 15, B

'Memo
LOCATE 3, 5
PRINT "QuickBasic : My Nostalgia"
LOCATE 3, 67
PRINT "2022.02.09"

'Body
LINE (wid% / 2 - 100, hei% / 2 - 100)-(wid% / 2 + 100, hei% / 2), 7, BF
LINE (wid% / 2 - 200, hei% / 2)-(wid% / 2 + 200, hei% / 2 + 100), 7, BF

'Windows
LINE (wid% / 2 - 100, hei% / 2 - 80)-(wid% / 2 - 60, hei% / 2), 9, BF
LINE (wid% / 2 - 50, hei% / 2 - 80)-(wid% / 2 - 5, hei% / 2), 9, BF
LINE (wid% / 2 + 5, hei% / 2 - 80)-(wid% / 2 + 50, hei% / 2), 9, BF
LINE (wid% / 2 + 60, hei% / 2 - 80)-(wid% / 2 + 100, hei% / 2), 9, BF

'Wheels
CIRCLE (wid% / 2 - 90, hei% / 2 + 100), 50, 8
CIRCLE (wid% / 2 + 90, hei% / 2 + 100), 50, 8
PAINT (wid% / 2 - 120, hei% / 2 + 100), 8, 8
PAINT (wid% / 2 + 120, hei% / 2 + 100), 8, 8
CIRCLE (wid% / 2 - 90, hei% / 2 + 100), 30, 7
CIRCLE (wid% / 2 + 90, hei% / 2 + 100), 30, 7
PAINT (wid% / 2 - 90, hei% / 2 + 100), 7, 7
PAINT (wid% / 2 + 90, hei% / 2 + 100), 7, 7

END