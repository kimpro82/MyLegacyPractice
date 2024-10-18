# [My Pascal Practice](/README.md#algol)

Even Github has forgotten this ancient language


### References

- [Wikipedia](https://en.wikipedia.org/) > [ALGOL](https://en.wikipedia.org/wiki/ALGOL)
- [Wikipedia](https://en.wikipedia.org/) > [ALGOL 68](https://en.wikipedia.org/wiki/ALGOL_68)
- [Algol 68 Genie](https://web.archive.org/web/20080109161728/http://www.xs4all.nl/~jmvdveer/syntax.html#formats)
- [JDoodle](https://www.jdoodle.com/) > [Online Algol 68 IDE](https://www.jdoodle.com/execute-algol68-online)


### \<List>

- [Multiplication Table (2024.10.18)](#multiplication-table-20241017)


## [Multiplication Table (2024.10.18)](#list)

- My initial trial to code in ALGOL (with [Perplexity](https://www.perplexity.ai/))
- Run with Genie 3.4.2 in [JDoodle](https://www.jdoodle.com/execute-algol68-online)
- To my surprise, ALGOL is not supported in Github language statistics!
- Code and Output
  <details>
    <summary>Code : multiplicationTable.a68</summary>

    ```alg
    BEGIN
        FOR group FROM 0 TO 2 DO
            FOR row FROM 1 TO 9 DO
                FOR col FROM 1 TO 3 DO
                    INT num := group * 3 + col;
                    INT result := num * row;
                    # printf(($d" * "d" = "2z"   "$, num, row, result)) #       # Unexpected results #
                    printf(($d" * "$, num, $d" = "$, row, $2z"   "$, result))   # Ok #
                OD;
                print(newline)
            OD;
            IF group < 2 THEN print(newline) FI
        OD
    END
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```txt
    1 * 1 =  1   2 * 1 =  2   3 * 1 =  3   
    1 * 2 =  2   2 * 2 =  4   3 * 2 =  6   
    1 * 3 =  3   2 * 3 =  6   3 * 3 =  9   
    ……
    1 * 9 =  9   2 * 9 = 18   3 * 9 = 27   

    4 * 1 =  4   5 * 1 =  5   6 * 1 =  6   
    4 * 2 =  8   5 * 2 = 10   6 * 2 = 12   
    4 * 3 = 12   5 * 3 = 15   6 * 3 = 18   
    ……
    4 * 9 = 36   5 * 9 = 45   6 * 9 = 54   

    7 * 1 =  7   8 * 1 =  8   9 * 1 =  9   
    7 * 2 = 14   8 * 2 = 16   9 * 2 = 18   
    7 * 3 = 21   8 * 3 = 24   9 * 3 = 27   
    ……  
    7 * 9 = 63   8 * 9 = 72   9 * 9 = 81 
    ```
  </details>
