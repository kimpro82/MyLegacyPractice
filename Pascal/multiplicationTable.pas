// 2024.10.17

program MultiplicationTable;

const
  MAX_TABLE = 9;
  MAX_MULTIPLICAND = 9;
  TABLES_PER_GROUP = 3;

var
  i, j, k, result: Integer;
  currentTable: Integer;

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
