--  Advent of Code 2015
--  John Perry
--
--  find all the ways to fill Capacity with eggnog

pragma Ada_2022;

with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Text_IO;
with Ada.Containers;
with Ada.Containers.Vectors;

procedure Day17 is

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;
   --  yeah, this one gave me a bit of trouble...

   subtype Container_Range is
     Positive range 1 .. (if Doing_Example then 5 else 20);
   type Capacity_Array is array (Container_Range) of Natural;
   Capacity : Capacity_Array;
   --  how much eggnog each container can hold
   Sums     : Capacity_Array := [others => 0];
   --  if the amount of eggnog remaining is more than
   --  the remaining containers' sum, then this combination is already invalid

   Total_Eggnog : constant Positive := (if Doing_Example then 25 else 150);

   --  SECTION
   --  I/O

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Natural);

   procedure Read_Input is
      Input : IO.File_Type;
      procedure Sortem is new
        Ada.Containers.Generic_Constrained_Array_Sort
          (Index_Type   => Container_Range,
           Element_Type => Positive,
           Array_Type   => Capacity_Array,
           "<"          => ">");
      --  note that we sort them from largest to smallest
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      for Ith in Capacity'Range loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive;
         begin
            Positive_IO.Get (Line, Capacity (Ith), Position);
         end;
      end loop;
      IO.Close (Input);
      --  sort from largest to smallest
      Sortem (Capacity);
      --  set up the optimization
      for Ith in Container_Range'Range loop
         Sums (Ith) := Capacity (Ith .. Container_Range'Last)'Reduce ("+", 0);
      end loop;
   end Read_Input;

   --  SECTION
   --  parts 1 (and setup for 2)

   --  SUBSECTION
   --  tracking the partitions for part 2

   type Partition_Array is array (Container_Range) of Boolean;

   package Partition_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Partition_Array);

   Successful_Partitions : Partition_Vectors.Vector;

   function Sum (Partition : Partition_Array) return Natural
   is ([for Each of Partition => (if Each then 1 else 0)]'Reduce ("+", 0));

   --  SUBSECTION
   --  recursive "descent", if that be the word

   function Descend_On
     (Ith       : Container_Range;
      Partition : Partition_Array := [others => False];
      Remaining : Natural := Total_Eggnog) return Natural
   is
      Result        : Natural := 0;
      New_Partition : Partition_Array := Partition;
   begin
      if Remaining = 0 then
         --  base case with success: record the partition and the count
         Result := 1;
         Successful_Partitions.Append (New_Partition);
      else
         for Jth in Ith .. Capacity'Last loop
            --  loop through the remaining containers, starting from current
            if Jth < Container_Range'Last
              and then Capacity (Jth) <= Remaining
              and then Sums (Jth) >= Remaining
            then
               --  it's not the last container, we can fill this container,
               --  and remaining containers might be able to cover the rest:
               --  so, set this container, then proceed to the next, then unset
               New_Partition (Jth) := True;
               Result :=
                 @
                 + Descend_On
                     (Jth + 1, New_Partition, Remaining - Capacity (Jth));
               New_Partition (Jth) := False;
            elsif Jth = Container_Range'Last
              and then Remaining = Capacity (Jth)
            then
               --  the last container consumes all the remaining nog:
               --  record the partition and increase the count
               Result := @ + 1;
               New_Partition (Jth) := True;
               Successful_Partitions.Append (New_Partition);
               New_Partition (Jth) := False;
            end if;
         end loop;
      end if;
      return Result;
   end Descend_On;

   --  SUBSECTION
   --  wrapper / starter for recursion

   function Part_1 return Natural
   is (Descend_On (1));
   --  this may be the shortest Part_1 i've ever written

   --  SECTION
   --  part 2

   function Part_2 return Natural is
      Result           : Natural := 0;
      Minimum          : Natural := Natural'Last;
      Possible_Minimum : Natural;
   begin
      --  look through the saved partitions to count the minimums
      for Partition of Successful_Partitions loop
         Possible_Minimum := Sum (Partition);
         if Possible_Minimum < Minimum then
            Result := 1;
            Minimum := Possible_Minimum;
         elsif Possible_Minimum = Minimum then
            Result := @ + 1;
         end if;
      end loop;
      return Result;
   end Part_2;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("There are" & Part_1'Image & " ways to combine the containers");
   IO.Put_Line ("There are" & Part_2'Image & " ways to fill the minimum");
end Day17;
