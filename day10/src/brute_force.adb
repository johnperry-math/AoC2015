--  Advent of Code 2015
--  John Perry
--
--  implementation for brute force solution to problem

pragma Ada_2022;

with Common;

package body Brute_Force is

   subtype Positive_Vector is Common.Positive_Vector;

   function Solution (Iterations : Positive) return Positive is
      Sequence : Positive_Vector := Common.Puzzle_Input;
   begin
      --  this is the naive, brute-force approach:
      --  create a new sequence; append new values to it
      --  according to the rules of look-and-say; then
      --  replace the old sequence with the new sequence
      for Each in 1 .. Iterations loop
         declare
            New_Sequence : Positive_Vector;
            Start, Stop  : Positive := Sequence.First_Index;
         begin
            while Start <= Sequence.Last_Index loop
               --  first find where the number currently indexed ends
               Stop := Start;
               while Stop < Sequence.Last_Index
                 and then Sequence (Stop + 1) = Sequence (Start)
               loop
                  Stop := @ + 1;
               end loop;
               --  add the count
               New_Sequence.Append (Stop - Start + 1);
               --  and then the number itself
               New_Sequence.Append (Sequence (Start));
               Start := Stop + 1;
            end loop;
            Sequence := New_Sequence;
         end;
      end loop;
      return Positive (Sequence.Length);
   end Solution;

end Brute_Force;
