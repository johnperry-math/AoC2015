--  Advent of Code 2015
--  John Perry
--
--  Help the elves determine how long their "look-and-say" sequences will be.

pragma Ada_2022;

with Ada.Text_IO;

with Atomic;
with Brute_Force;
with Common;

procedure Day10 is
   package IO renames Ada.Text_IO;

   --  SECTION
   --  main

begin
   IO.Put_Line ("Atomic expansion first!");
   declare
      Count : Atomic.Elem_Count_Array := [others => 0];
   begin
      Count (Atomic.Identify_Atom_For (Common.Puzzle_Input)) := 1;
      for Each in 1 .. 40 loop
         Count := Atomic.Decay (Count);
      end loop;
      IO.Put_Line
        ("After 40 iterations, the length of the sequence is"
         & Atomic.Expanded_Length (Count)'Image);
      for Each in 1 .. 10 loop
         Count := Atomic.Decay (Count);
      end loop;
      IO.Put_Line
        ("After 50 iterations, the length of the sequence is"
         & Atomic.Expanded_Length (Count)'Image);
      for each in 1 .. 50 loop
         Count := Atomic.Decay (Count);
      end loop;
      IO.Put_Line
        ("After 100 iterations, the length of the sequence is"
         & Atomic.Expanded_Length (Count)'Image);
   end;

   IO.Put_Line ("Now, for slow, brute force!");
   IO.Put_Line
     ("After 40 iterations, the length of the sequence is"
      & Brute_Force.Solution (40)'Image);
   IO.Put_Line
     ("After 50 iterations, the length of the sequence is"
      & Brute_Force.Solution (50)'Image);
end Day10;
