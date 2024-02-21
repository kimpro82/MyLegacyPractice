# [My Ada Practice](/README.md#ada)

Remember Ada Lovelace


### \<List>

- [Multiplication Table (2024.02.21)](#multiplication-table-20240221)
- [Hello World (2024.02.21)](#hello-world-20240221)


## [Multiplication Table (2024.02.21)](#list)

- Compiler ☞ [JDoodle](https://www.jdoodle.com/) > [Online Ada Compiler](https://www.jdoodle.com/execute-ada-online)
- Code and Output
  <details>
    <summary>Code : Multiplication_Table.adb</summary>

    ```ada
    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
    ```
    ```ada
    procedure Multiplication_Table is
    -- warning: file name does not match unit name, should be "multiplication_table.adb" [enabled by default]
    begin
        for Group_Index in 0..2 loop
            for Multiplier in 1..9 loop
                for Position_In_Group in 1..3 loop
                    declare
                        Multiplicand : Integer := Group_Index * 3 + Position_In_Group;
                        Result : Integer := Multiplicand * Multiplier;
                    begin
                        Put(Multiplicand, 1);
                        Put(" * ");
                        Put(Multiplier, 1);
                        Put(" = ");
                        Put(Result, 2);
                        Put("   ");
                    end;
                end loop;
                New_Line;
            end loop;
            New_Line;
        end loop;
    end Multiplication_Table;
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```ada
    1 * 1 =  1   2 * 1 =  2   3 * 1 =  3   
    1 * 2 =  2   2 * 2 =  4   3 * 2 =  6   
    1 * 3 =  3   2 * 3 =  6   3 * 3 =  9   
    1 * 4 =  4   2 * 4 =  8   3 * 4 = 12   
    1 * 5 =  5   2 * 5 = 10   3 * 5 = 15   
    1 * 6 =  6   2 * 6 = 12   3 * 6 = 18   
    1 * 7 =  7   2 * 7 = 14   3 * 7 = 21   
    1 * 8 =  8   2 * 8 = 16   3 * 8 = 24   
    1 * 9 =  9   2 * 9 = 18   3 * 9 = 27   

    4 * 1 =  4   5 * 1 =  5   6 * 1 =  6   
    4 * 2 =  8   5 * 2 = 10   6 * 2 = 12   
    4 * 3 = 12   5 * 3 = 15   6 * 3 = 18   
    4 * 4 = 16   5 * 4 = 20   6 * 4 = 24   
    4 * 5 = 20   5 * 5 = 25   6 * 5 = 30   
    4 * 6 = 24   5 * 6 = 30   6 * 6 = 36   
    4 * 7 = 28   5 * 7 = 35   6 * 7 = 42   
    4 * 8 = 32   5 * 8 = 40   6 * 8 = 48   
    4 * 9 = 36   5 * 9 = 45   6 * 9 = 54   

    7 * 1 =  7   8 * 1 =  8   9 * 1 =  9   
    7 * 2 = 14   8 * 2 = 16   9 * 2 = 18   
    7 * 3 = 21   8 * 3 = 24   9 * 3 = 27   
    7 * 4 = 28   8 * 4 = 32   9 * 4 = 36   
    7 * 5 = 35   8 * 5 = 40   9 * 5 = 45   
    7 * 6 = 42   8 * 6 = 48   9 * 6 = 54   
    7 * 7 = 49   8 * 7 = 56   9 * 7 = 63   
    7 * 8 = 56   8 * 8 = 64   9 * 8 = 72   
    7 * 9 = 63   8 * 9 = 72   9 * 9 = 81   
    ```
  </details>


## [Hello World (2024.02.21)](#list)

- My initial Ada code
- Compiler ☞ [JDoodle](https://www.jdoodle.com/) > [Online Ada Compiler](https://www.jdoodle.com/execute-ada-online)
- Code and Output
  <details open="">
    <summary>Code : Hello_World.adb</summary>

    ```ada
    with Text_IO; use Text_IO;
    ```
    ```ada
    procedure Hello_World is

    begin
      Put_Line("Remember Ada Lovelace");
    end Hello_World;
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```ada
    Remember Ada Lovelace
    ```
  </details>
