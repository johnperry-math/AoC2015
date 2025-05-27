--  Advent of Code 2015
--  John Perry
--
--  Determine which strings are naughty or nice.

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day05 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   subtype File_String is String (1 .. 16);

   package File_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => File_String);

   Candidates : File_String_Vectors.Vector;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         Candidates.Append (IO.Get_Line (Input));
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  Part 1

   function Is_Vowel (C : Character) return Boolean
   is (C = 'a'
       or else C = 'e'
       or else C = 'i'
       or else C = 'o'
       or else C = 'u');

   function At_Least_Three_Vowels (Candidate : File_String) return Boolean
   is ([for C of Candidate => (if Is_Vowel (C) then 1 else 0)]'Reduce ("+", 0)
       >= 3);

   function At_Least_One_Repeated_Letter
     (Candidate : File_String) return Boolean
   is (for some Ith in Candidate'First .. Candidate'Last - 1
       => Candidate (Ith) = Candidate (Ith + 1));

   function All_Pairs_Valid (Candidate : File_String) return Boolean
   is (for all Ith in Candidate'First .. Candidate'Last - 1
       => Candidate (Ith .. Ith + 1) /= "ab"
          and then Candidate (Ith .. Ith + 1) /= "cd"
          and then Candidate (Ith .. Ith + 1) /= "pq"
          and then Candidate (Ith .. Ith + 1) /= "xy");

   function Is_Nice (Candidate : File_String) return Boolean
   is (At_Least_Three_Vowels (Candidate)
       and then At_Least_One_Repeated_Letter (Candidate)
       and then All_Pairs_Valid (Candidate));

   function Part_1 return Natural
   is ([for Candidate of Candidates => (if Is_Nice (Candidate) then 1 else 0)]'
         Reduce ("+", 0));

   --  SECTION
   --  Part 2

   function At_Least_One_Nonoverlapping_Pair
     (Candidate : File_String) return Boolean
   is (for some Ith in Candidate'First .. Candidate'Last - 2
       => (for some Jth in Ith + 2 .. Candidate'Last - 1
           => Candidate (Ith .. Ith + 1) = Candidate (Jth .. Jth + 1)));

   function At_Least_One_Near_Repetition
     (Candidate : File_String) return Boolean
   is (for some Ith in Candidate'First .. Candidate'Last - 2
       => Candidate (Ith) = Candidate (Ith + 2));

   function Part_2 return Natural
   is ([for Candidate of Candidates
        => (if At_Least_One_Nonoverlapping_Pair (Candidate)
              and then At_Least_One_Near_Repetition (Candidate)
            then 1
            else 0)]'Reduce ("+", 0));

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line ("There are" & Part_1'Image & " nice strings.");
   IO.Put_Line
     ("After reconsideration, there are" & Part_2'Image & " nice strings.");
end Day05;
