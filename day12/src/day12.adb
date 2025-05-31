--  Advent of Code 2015
--  John Perry
--
--  help the elves balance their books (in more ways than one...)

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Characters.Handling;

procedure Day12 is

   package IO renames Ada.Text_IO;
   package Integer_IO is new IO.Integer_IO (Num => Integer);
   package Chars renames Ada.Characters.Handling;

   --  SECTION
   --  part 1

   function Sum_Numbers (Line : String) return Integer is
      --  sums the numbers in Line
      Result   : Integer := 0;
      Position : Positive := Line'First;
      Value    : Integer;
   begin
      while Position <= Line'Last loop
         if Line (Position) = '-' or else Chars.Is_Digit (Line (Position)) then
            Integer_IO.Get (Line (Position .. Line'Last), Value, Position);
            Result := @ + Value;
         end if;
         Position := @ + 1;
      end loop;
      return Result;
   end Sum_Numbers;

   function Part_1 return Integer is
      --  applies Sum_Numbers to all the lines in "input.txt"
      Input  : IO.File_Type;
      Result : Integer := 0;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            Result := @ + Sum_Numbers (Line);
         end;
      end loop;
      IO.Close (Input);
      return Result;
   end Part_1;

   function Balance (Line : String) return Positive is
      --  assuming that the position before Line'First was {,
      --  returns the position of the } that balances it
      Position : Positive := Line'First;
      Balance  : Natural := 1;
   begin
      while Balance > 0 loop
         if Line (Position) = '{' then
            Balance := @ + 1;
         elsif Line (Position) = '}' then
            Balance := @ - 1;
         end if;
         Position := @ + 1;
      end loop;
      return Position - 1;
   end Balance;

   function Sum_Non_Red (Line : String) return Integer is
      --  assuming Line is a JSON string and
      --  the previous (or first) character is {,
      --  sums the numbers in all objects  that do not have a property of "red"
      Position     : Positive := Line'First;
      Result       : Integer := 0;
      Value        : Integer;
      Old_Position : Positive;
   begin
      while Position < Line'Last loop
         if Line (Position) = '{' then
            --  new object; let's find its sum recursively, then jump past it
            Old_Position := Position;
            Result := @ + Sum_Non_Red (Line (Position + 1 .. Line'Last));
            Position := Balance (Line (Position + 1 .. Line'Last));
         elsif Line (Position) = '}' then
            --  we've hit the end of our group
            exit;
         elsif Position < Line'Last - 6
           and then Line (Position .. Position + 5) = ":""red"""
         then
            --  we have a red property; don't count it
            return 0;
         elsif Line (Position) = '-' or else Chars.Is_Digit (Line (Position))
         then
            --  we have a number
            Integer_IO.Get (Line (Position .. Line'Last), Value, Position);
            Result := @ + Value;
         end if;
         Position := @ + 1;
      end loop;
      return Result;
   end Sum_Non_Red;

   function Part_2 return Integer is
      --  applies Sum_Non_Red to all the lines in "input.txt"
      Input  : IO.File_Type;
      Result : Integer := 0;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            Result := @ + Sum_Non_Red (Line);
         end;
      end loop;
      IO.Close (Input);
      return Result;
   end Part_2;

begin
   IO.Put_Line ("The sum is" & Part_1'Image);
   IO.Put_Line ("The corrected sum is" & Part_2'Image);
end Day12;
