--  Advent of Code 2015
--  John Perry
--
--  help Santa boot up his weather machine

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day25 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   package Positive_IO is new IO.Integer_IO (Num => Positive);

   --  SECTION
   --  global types and variables

   type Mod_33554393 is mod 33_554_393;

   First_Code : constant Mod_33554393 := 20_151_125;

   Multiple : constant Mod_33554393 := 252_533;

   function Next_Code (Current : Mod_33554393) return Mod_33554393
   is (Current * Multiple);

   Target_Row, Target_Col : Positive;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         Line     : String := IO.Get_Line (Input);
         Position : Positive;
      begin
         Positive_IO.Get (Line (81 .. Line'Last), Target_Row, Position);
         Positive_IO.Get (Line (94 .. Line'Last), Target_Col, Position);
      end;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 (and 2! for free! as usual)

   function Solution return Mod_33554393 is
      --  brute force :-/
      Row, Col  : Positive := 1;
      Result    : Mod_33554393 := First_Code;
      Iteration : Positive := 1;
   begin
      while Row /= Target_Row or else Col /= Target_Col loop
         if Iteration < 21 then
            Iteration := @ + 1;
         end if;
         if Row = 1 then
            Row := Col + 1;
            Col := 1;
         else
            Row := @ - 1;
            Col := @ + 1;
         end if;
         Result := Next_Code (Result);
      end loop;
      return Result;
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line ("The code is" & Solution'Image);
end Day25;
