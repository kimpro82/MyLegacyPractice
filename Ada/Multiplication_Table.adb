-- Multiplication Table
-- 2024.02.21

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

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
