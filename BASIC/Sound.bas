SHELL "CLS"

'BEEP
PRINT "BEEP"
BEEP
PRINT CHR$(7)
SLEEP

'SOUND
PRINT "SOUND" + CHR$(13) 'CHR$(13) : Line break
FOR i% = 1 TO 30
        SOUND i% * 100, 1  'Frequency, Duration
NEXT
SLEEP

'PLAY
PRINT "PLAY" + CHR$(13)
PLAY "L16 CDEFGAB>C" '> : Move up one octave
SLEEP
