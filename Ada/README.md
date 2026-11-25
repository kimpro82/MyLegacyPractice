# [My Ada Practice](/README.md#ada)

Remember Ada Lovelace


### References

- [Wikibooks](https://en.wikibooks.org/)
  - [Ada Programming](https://en.wikibooks.org/wiki/Ada_Programming)  
  - [Ada Style Guide](https://en.wikibooks.org/wiki/Ada_Style_Guide)
- Compiler : [JDoodle](https://www.jdoodle.com/) > [Online Ada Compiler](https://www.jdoodle.com/execute-ada-online)


### \<Note>

- Run an Ada program with `./run.sh Ada/Strong_Typing.adb` from the repository root, or `./run.sh Strong_Typing.adb` from this directory.
- On Debian/Ubuntu, install GNAT if needed with `./install_env.sh`.


### \<List>

- [Strong Typing (2026.11.25)](#strong-typing-20261125)
- [Multiplication Table (2024.02.21)](#multiplication-table-20240221)
- [Hello World (2024.02.21)](#hello-world-20240221)



## [Strong Typing (2026.11.25)](#list)

- Practice strong typing with a flight-monitoring example:
  - `Feet`, `Meters`, and `Knots` are distinct numeric types.
  - `Cruise_Altitude` and `Operational_Airspeed` enforce valid ranges.
  - Conversion functions make unit changes explicit.
  - Out-of-range assignments raise and handle `Constraint_Error`.
- Code and Output
  <details>
    <summary>Code : Strong_Typing.adb</summary>

    ```ada
    -- Strong Typing
    -- Date : 2026.11.25
    -- Demonstrates distinct numeric types, range constraints, and explicit
    -- unit conversions in a flight-monitoring example.

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Strong_Typing is
       -- Distinct types prevent accidental mixing of different units.
       type Feet is new Integer;
       type Meters is new Integer;
       type Knots is new Float;

       -- Subtypes constrain values to safe operating ranges.
       subtype Cruise_Altitude is Feet range 10_000 .. 45_000;
       subtype Operational_Airspeed is Knots range 0.0 .. 450.0;

       -- Convert through Float to support fractional unit scaling.
       function To_Feet (Value : Meters) return Feet is
       begin
          return Feet (Float (Value) * 3.28084);
       end To_Feet;

       -- Convert back to whole meters.
       function To_Meters (Value : Feet) return Meters is
       begin
          return Meters (Float (Value) / 3.28084);
       end To_Meters;

       Measured_Altitude : Meters := 10_000;
       Cruise_Level      : Cruise_Altitude := To_Feet (Measured_Altitude);
       Converted_Altitude : Meters := To_Meters (Cruise_Level);
       Airspeed          : Operational_Airspeed := 250.0;
    begin
       Put_Line ("Altitude in feet:" &
         Integer'Image (Integer (Cruise_Level)));
       Put_Line ("Converted altitude in meters:" &
         Integer'Image (Integer (Converted_Altitude)));
       Put_Line ("Airspeed in knots:" &
         Float'Image (Float (Airspeed)));

       -- Out-of-range assignments raise Constraint_Error; catch it for the demo.
       begin
          Cruise_Level := Feet (50_000);
       exception
          when Constraint_Error =>
             Put_Line ("Rejected altitude outside the cruise range.");
       end;

       begin
          Airspeed := Knots (500.0);
       exception
          when Constraint_Error =>
             Put_Line ("Rejected airspeed outside the operational range.");
       end;
    end Strong_Typing;
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```text
    gnatmake: "Strong_Typing" up to date.

    Altitude in feet: 32808
    Converted altitude in meters: 10000
    Airspeed in knots: 2.50000E+02
    Rejected altitude outside the cruise range.
    Rejected airspeed outside the operational range.
    ```
  </details>



## [Multiplication Table (2024.02.21)](#list)

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

- My initial *Ada* code
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
