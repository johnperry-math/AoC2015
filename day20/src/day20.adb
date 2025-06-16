--  Advent of Code 2015
--  John Perry
--
--  help the elves figure out which houses receive how many presents

pragma Ada_2022;

with Ada.Text_IO;

procedure Day20 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types, variables, and constants

   Puzzle_Input : constant Positive := 36_000_000;

   --  SECTION
   --  parts 1 and 2

   Badness : exception;

   function Solution
     (Multiple : Positive; Limit : Positive := Positive'Last) return Positive
   is
      Gifts : array (1 .. 1_000_000) of Positive := [others => Multiple];
   begin
      --  first the elves leave gifts at their respective houses
      for Elf in 2 .. Gifts'Last loop
         declare
            Current : Positive := Elf;
            Stop_At : constant Positive :=
              (if Limit = Positive'Last
               then Gifts'Last
               else Positive'Min (Gifts'Last, Elf * (Limit - 1)));
         begin
            while Current <= Stop_At loop
               Gifts (Current) := @ + Multiple * Elf;
               Current := @ + Elf;
            end loop;
         end;
      end loop;
      --  now look for the first house to receive at least Puzzle_Input gifts
      for Result in Gifts'Range loop
         if Gifts (Result) >= Puzzle_Input then
            return Result;
         end if;
      end loop;
      raise Badness;
   end Solution;

   --  SECTION
   --  main

begin
   IO.Put_Line
     ("The first house to receive at least"
      & Puzzle_Input'Image
      & " presents is"
      & Solution (Multiple => 10)'Image);
   IO.Put_Line
     ("After elves change their minds, the first house to receive at least"
      & Puzzle_Input'Image
      & " presents is"
      & Solution (Multiple => 11, Limit => 50)'Image);
end Day20;
