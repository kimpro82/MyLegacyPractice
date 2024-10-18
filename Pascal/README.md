# [My Pascal Practice](/README.md#pascal)

……


### References

- [Wikipedia](https://en.wikipedia.org/) > [Pascal (programming language)](https://en.wikipedia.org/wiki/Pascal_(programming_language))
- [JDoodle](https://www.jdoodle.com/) > [Online Pascal Compiler](https://www.jdoodle.com/execute-pascal-online)


### \<List>

- [Multiplication Table (2024.10.17)](#multiplication-table-20241017)


## [Multiplication Table (2024.10.17)](#list)

- My initial trial to code in Pascal (with [Perplexity](https://www.perplexity.ai/))
- Run with Free Pascal Compiler 3.2.2 in [JDoodle](https://www.jdoodle.com/execute-pascal-online)
- Code and Output
  <details>
    <summary>Code : multiplicationTable.pas</summary>

    ```pas
    program MultiplicationTable;
    ```
    ```pas
    const
      MAX_TABLE = 9;
      MAX_MULTIPLICAND = 9;
      TABLES_PER_GROUP = 3;
    ```
    ```pas
    var
      i, j, k, result: Integer;
      currentTable: Integer;
    ```
    ```pas
    begin
      for i := 1 to MAX_TABLE div TABLES_PER_GROUP do
      begin
        for j := 1 to MAX_MULTIPLICAND do
        begin
          for k := 1 to TABLES_PER_GROUP do
          begin
            currentTable := (i - 1) * TABLES_PER_GROUP + k;
            result := currentTable * j;

            write(currentTable:2, ' * ', j:1, ' = ');
            if result < 10 then
              write('  ', result:1)
            else
              write(' ', result:2);

            if k < TABLES_PER_GROUP then
              write('   ');
          end;
          writeln;
        end;
        writeln;
      end;
    end.
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```ada
     1 * 1 =   1    2 * 1 =   2    3 * 1 =   3
     1 * 2 =   2    2 * 2 =   4    3 * 2 =   6
     1 * 3 =   3    2 * 3 =   6    3 * 3 =   9
     ……
     1 * 9 =   9    2 * 9 =  18    3 * 9 =  27

     4 * 1 =   4    5 * 1 =   5    6 * 1 =   6
     4 * 2 =   8    5 * 2 =  10    6 * 2 =  12
     4 * 3 =  12    5 * 3 =  15    6 * 3 =  18
     ……
     4 * 9 =  36    5 * 9 =  45    6 * 9 =  54

     7 * 1 =   7    8 * 1 =   8    9 * 1 =   9
     7 * 2 =  14    8 * 2 =  16    9 * 2 =  18
     7 * 3 =  21    8 * 3 =  24    9 * 3 =  27
     7 * 4 =  28    8 * 4 =  32    9 * 4 =  36
     ……
     7 * 9 =  63    8 * 9 =  72    9 * 9 =  81
    ```
  </details>
