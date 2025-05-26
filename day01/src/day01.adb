--  2015 Advent of Code in Ada, Day 01 (in SPARK!)
--  John Perry
--
--  Santa is confused by an elevator.
--
--  MAIN PROGRAM

pragma Ada_2022;

with Ada.Text_IO;

with Support;

procedure Day01 is

   package IO renames Ada.Text_IO;

   subtype Direction is Support.Direction;
   use all type Direction;

   use all type Support.Direction_Vector;

   --  SECTION: global types, variables, and utilities

   Directions : Support.Direction_Vector;

   Invalid_Character : exception;

   function To_Direction
     (Value : Character)
      return Direction
             --  converts Value to a Direction
   is (case Value is
         when '(' => Up,
         when ')' => Dn,
         when others => raise Invalid_Character with Value'Image);

   --  SECTION: I/O

   procedure Read_Input is
      --  reads the one input line and stores it in Directions
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, "input.txt");
      declare
         Line     : String := IO.Get_Line (File);
         Position : Positive := Line'First;
      begin
         while Position <= Line'Last loop
            Append (Directions, To_Direction (Line (Position)));
            Position := @ + 1;
         end loop;
      end;
      IO.Close (File);
   end Read_Input;

   --  SECTION: main

begin
   Read_Input;
   IO.Put_Line
     ("The instructions take Santa to floor"
      & Support.Part_1 (Directions)'Image);
   IO.Put_Line
     ("He enters the basement on the"
      & Support.Part_2 (Directions)'Image
      & "th instruction");
end Day01;
