--  Advent of Code 2015
--  John Perry
--
--  Help the elves determine how long their "look-and-say" sequences will be.

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day10 is
   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   package Positive_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);

   subtype Positive_Vector is Positive_Vecs.Vector;

   Puzzle_Input : constant Positive_Vector := [3, 1, 1, 3, 3, 2, 2, 1, 1, 3];

   --  SECTION
   --  solution

   function Solution (Iterations : Positive) return Positive is
      Sequence : Positive_Vector := Puzzle_Input;
   begin
      for Each in 1 .. Iterations loop
         declare
            New_Sequence : Positive_Vector;
            Start, Stop  : Positive := Sequence.First_Index;
         begin
            while Start <= Sequence.Last_Index loop
               Stop := Start;
               while Stop < Sequence.Last_Index
                 and then Sequence (Stop + 1) = Sequence (Start)
               loop
                  Stop := @ + 1;
               end loop;
               New_Sequence.Append (Stop - Start + 1);
               New_Sequence.Append (Sequence (Start));
               Start := Stop + 1;
            end loop;
            Sequence := New_Sequence;
         end;
      end loop;
      return Positive (Sequence.Length);
   end Solution;

   --  SECTION
   --  main

begin
   IO.Put_Line
     ("After 40 iterations, the length of the sequence is"
      & Solution (40)'Image);
   IO.Put_Line
     ("After 50 iterations, the length of the sequence is"
      & Solution (50)'Image);
end Day10;
