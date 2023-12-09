'-10 print "-10"        ' A negative line number causes an error
0 CLS
10 PRINT "10"
15 GOTO 30
20 PRINT "20"           ' Pass
30 PRINT "30"
25 PRINT "25"           ' Decreasing numbering is OK
30.5 PRINT "30.5"       ' Decimal point is allowed
A: GOTO C
B: PRINT "B"
C: PRINT "C"            ' Alphanumeric line labels can be mixed
PRINT "No label"        ' Lines without labeling is also available
65535 PRINT "65535"
65536 PRINT "65536"     ' The line number can be over 65536
