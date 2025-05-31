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
      Sequence : Atomic.Elem_Vector :=
        [Atomic.Identify_Atom_For (Common.Puzzle_Input)];
   begin
      for Each in 1 .. 40 loop
         Sequence := Atomic.Decay (Sequence);
      end loop;
      IO.Put_Line
        ("After 40 iterations, the length of the sequence is"
         & Atomic.Expanded_Length (Sequence)'Image);
      for Each in 1 .. 10 loop
         Sequence := Atomic.Decay (Sequence);
      end loop;
      IO.Put_Line
        ("After 50 iterations, the length of the sequence is"
         & Atomic.Expanded_Length (Sequence)'Image);
   end;

   IO.Put_Line ("Now, for slow, brute force!");
   IO.Put_Line
     ("After 40 iterations, the length of the sequence is"
      & Brute_Force.Solution (40)'Image);
   IO.Put_Line
     ("After 50 iterations, the length of the sequence is"
      & Brute_Force.Solution (50)'Image);
end Day10;
