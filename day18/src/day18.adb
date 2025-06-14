--  Advent of Code 2015
--  John Perry
--
-- the Christmas lights play the Game of Life

pragma Ada_2022;

with Ada.Text_IO;

procedure Day18 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   type Light_Enum is (On, Off);

   type Light_Arrangement is array (1 .. 100, 1 .. 100) of Light_Enum;

   Initial_Arrangement : Light_Arrangement := [others => [others => Off]];

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for Row in Initial_Arrangement'Range (1) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Initial_Arrangement'Range (2) loop
               if Line (Col) = '#' then
                  Initial_Arrangement (Row, Col) := On;
               end if;
            end loop;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  support functions

   function Count_Lit_Neighbors
     (Arrangement : Light_Arrangement; Row, Col : Positive) return Natural
   is
      Result : Natural := 0;
   begin
      for Row_Offset in -1 .. 1 when Row + Row_Offset in Arrangement'Range (1)
      loop
         for Col_Offset
           in -1 .. 1
           when Col + Col_Offset in Arrangement'Range (2)
         loop
            if (Row_Offset /= 0 or else Col_Offset /= 0)
              and then Arrangement (Row + Row_Offset, Col + Col_Offset) = On
            then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_Lit_Neighbors;

   type Part_Enum is (First, Second);

   function Is_Corner (Row, Col : Positive) return Boolean
   is ((Row = 1 and then Col = 1)
       or else (Row = 1 and then Col = 100)
       or else (Row = 100 and then Col = 1)
       or else (Row = 100 and then Col = 100));

   function Update_To
     (Arrangement : Light_Arrangement; Row, Col : Positive; Part : Part_Enum)
      return Light_Enum
   is
      Neighbors_Lit : constant Natural :=
        Count_Lit_Neighbors (Arrangement, Row, Col);
   begin
      return
        (if Part = Second and then Is_Corner (Row, Col)
         then On
         elsif Arrangement (Row, Col) = On
         then (if Neighbors_Lit in 2 .. 3 then On else Off)
         elsif Neighbors_Lit = 3
         then On
         else Off);
   end Update_To;

   function Next
     (Arrangement : Light_Arrangement; Part : Part_Enum)
      return Light_Arrangement
   is ([for Row in Light_Arrangement'Range (1)
        => [for Col in Light_Arrangement'Range (2)
            => (Update_To (Arrangement, Row, Col, Part))]]);

   --  SUBSECTION
   --  solution, depending on part

   function Solution (Part : Part_Enum) return Natural is
      Current : Light_Arrangement := Initial_Arrangement;
      Result  : Natural := 0;
   begin
      --  this part really could have used more explanation,
      --  given that the example already had them lit, but
      --  my input didn't. fortunately I CHECKED FIRST
      if Part = Second then
         Current (1, 1) := On;
         Current (1, 100) := On;
         Current (100, 1) := On;
         Current (100, 100) := On;
      end if;
      for Each in 1 .. 100 loop
         Current := Next (Current, Part);
      end loop;
      for Row in Current'Range (1) loop
         for Col in Current'Range (2) loop
            if Current (Row, Col) = On then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("After 100 iterations," & Solution (First)'Image & " bulbs are lit");
   IO.Put_Line
     ("No, after reading additional instructions,"
      & Solution (Second)'Image
      & " bulbs are lit");
end Day18;
