--  Advent of Code 2015
--  John Perry
--
--  help Santa distribute the weight of packages evenly in his sleigh
--
--  the reader would probably appreciate some explanation of the algorithm:
--  * we read in the values
--  * we identify the subsets that add to a certain weight
--    (in part 1, total / 3; in part 2, total / 4)
--  * we store those subsets (lists, really) in a set AND in another list
--  * we sort the latter list according to number of elements, breaking ties
--    by "quantum entanglement" (defined in the puzzle)
--  * finally, we iterate through the latter list in order they were sorted,
--    taking the first element for which we can find 2 other elements
--    (in part 2, 3 other elements) that are also elements of
--    the set of all lists

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day24 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   use all type Ada.Containers.Count_Type;

   Debugging : constant Boolean := False;
   --  i found it useful to work the example on this one

   --  SECTION
   --  global packages, types, and variables

   --  SUBSECTION
   --  the entanglement value

   type Entanglement_Value is range 0 .. 2**96 - 1;

   --  SUBSECTION
   --  generic package instantiations

   --  SUBSUBSECTION
   --  Weight_Vector
   --  this is just a list of positive numbers,
   --  but we need to define a test for equality
   --  to build sets and vectors of them
   --  we also use Entanglement_Values instead of Positive
   --  to avoid type conversion later on

   package Weight_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Entanglement_Value);

   subtype Weight_Vector is Weight_Vectors.Vector;

   function "=" (Left, Right : Weight_Vector) return Boolean
   is (Natural (Left.Length) = Natural (Right.Length)
       and then (for all Ith in 1 .. Left.Last_Index
                 => Left (Ith) = Right (Ith)));

   --  SUBSUBSECTION
   --  Positive_Vector_Set
   --  we use this to "cache" previously discovered vectors
   --  that satisfy the constraints
   --  it requires a hasah function

   function Hash (Vector : Weight_Vector) return Ada.Containers.Hash_Type is
      type Mod_Z is mod 32003;
      Result : Mod_Z := 1;
   begin
      for Each of Vector loop
         Result := @ * Mod_Z (Each);
      end loop;
      return Ada.Containers.Hash_Type (Result);
   end Hash;

   package Weight_Vector_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Weight_Vector,
        Hash                => Hash,
        Equivalent_Elements => "=");
   subtype Positive_Vector_Set is Weight_Vector_Sets.Set;

   --  SUBSUBSECTION
   --  Weight_Vector_Vectors
   --  this is a list of Weight_Vector;
   --  essentially it's a copy of Weight_Vector_Sets,
   --  but placed in a list to make for
   --  (hopefully) quicker sorting and iteration

   package Weight_Vector_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Weight_Vector);
   subtype Positive_Vector_Vector is Weight_Vector_Vectors.Vector;

   function Quantum_Entanglement
     (Vector : Weight_Vector) return Entanglement_Value
   is
      Result : Entanglement_Value := 1;
   begin
      for Each of Vector loop
         Result := @ * Each;
      end loop;
      return Result;
   end Quantum_Entanglement;

   function "<" (Left, Right : Weight_Vector) return Boolean
   is (Left.Length < Right.Length
       or else (Left.Length = Right.Length
                and then Quantum_Entanglement (Left)
                         < Quantum_Entanglement (Right)));

   package Number_Then_Entanglement is new
     Weight_Vector_Vectors.Generic_Sorting;

   --  SUBSECTION
   --  global variables

   Package_Weights   : Weight_Vector;
   --  the weight of each package, in reverse order of input
   --  (they're given from smallest to largest and i found it useful
   --  to reorder them for a possibly questionable optimization)
   Remaining_Weights : Weight_Vector;
   --  for index I, Remaining_Weights(I) is the sum of all package weights
   --  from I onwards, **including** I

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input    : IO.File_Type;
      Filename : constant String :=
        (if Debugging then "example.txt" else "input.txt");
   begin
      IO.Open (Input, IO.In_File, Filename);
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive;
            Value    : Positive;
         begin
            Positive_IO.Get (Line, Value, Position);
            Package_Weights.Append (Entanglement_Value (Value));
         end;
      end loop;
      IO.Close (Input);
      --  the original input is in order from smallest to largest;
      --  we put them in order of largest to smallest...
      Package_Weights.Reverse_Elements;
      --  so that we can then sum the remaining weights, which we use later
      --  to prune paths that would be too small
      for Ith in 1 .. Package_Weights.Last_Index loop
         declare
            Remaining_Weight : Entanglement_Value := Package_Weights (Ith);
         begin
            for Jth in Ith + 1 .. Package_Weights.Last_Index loop
               Remaining_Weight := @ + Package_Weights (Jth);
            end loop;
            Remaining_Weights.Append (Remaining_Weight);
         end;
      end loop;
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   Potential_Distributions : Positive_Vector_Set;
   --  contains all the potential equal-weight distributions of packages
   --  into one part of the sleigh

   Not_Found : exception;
   --  this should never be raised!

   --  SUBSECTION
   --  find the distributions

   procedure Distribute_Weights_From
     (Ith           : Positive;
      Goal_Weight   : Entanglement_Value;
      Prev_Division : Weight_Vector;
      Prev_Weight   : Entanglement_Value := 0)
   is
      --  recursively finds distributions whose package weights
      --  sum to Goal_Weight, starting from the Ith position,
      --  given the previous division, which had the previous weight
      --  and the previous entanglement

      Curr_Division : Weight_Vector := Prev_Division;
      --  will contain all the weights considered so far, including this one

      This_Weight : constant Entanglement_Value := Package_Weights (Ith);
      Curr_Weight : constant Entanglement_Value := Prev_Weight + This_Weight;
      --  sum of all weights in Curr_Division

   begin

      Curr_Division.Append (This_Weight);

      if Curr_Weight = Goal_Weight then
         --  a happy end state! :-)
         if not Potential_Distributions.Contains (Curr_Division) then
            Potential_Distributions.Insert (Curr_Division);
         end if;

      else
         -- some useful debugging while working the example
         if Debugging then
            for Each of Curr_Division loop
               IO.Put (Each'Image);
            end loop;
            IO.New_Line;
            IO.Put_Line ("   " & Prev_Weight'Image & Curr_Weight'Image);
         end if;
         if not Potential_Distributions.Contains (Curr_Division) then
            --  if we haven't already considered this division,
            --  loop through the remaining weights, trying to add one when
            --  it would keep the total weight small enough, AND
            --  we can still find enough weight to reach the goal
            for Jth
              in Ith + 1 .. Package_Weights.Last_Index
              when Curr_Weight + Package_Weights (Jth) <= Goal_Weight
              and then Curr_Weight + Remaining_Weights (Jth) >= Goal_Weight
            loop
               Distribute_Weights_From
                 (Jth, Goal_Weight, Curr_Division, Curr_Weight);
            end loop;
         end if;
      end if;
   end Distribute_Weights_From;

   --  SUBSECTION
   --  part 1

   function Part_1 return Entanglement_Value is
      Goal_Weight      : constant Entanglement_Value :=
        Package_Weights'Reduce ("+", 0) / 3;
      Division         : Weight_Vector;
      Vectors_In_Order : Positive_Vector_Vector;
   begin

      IO.Put_Line ("Distributing");
      for Ith in 1 .. Package_Weights.Last_Index loop
         Distribute_Weights_From (Ith, Goal_Weight, Division, 0);
      end loop;

      IO.Put_Line ("Sorting");
      for Each of Potential_Distributions loop
         Vectors_In_Order.Append (Each);
      end loop;
      Number_Then_Entanglement.Sort (Vectors_In_Order);

      IO.Put_Line ("Searching");
      --  find the first distribution for which we can find two others
      --  that reach the goal weight
      for Ith in 1 .. Natural (Vectors_In_Order.Length) loop

         --  we have to check for a second, but a third is forced by two;
         --  we also have to make sure the first two don't overlap
         for Jth
           in Ith + 1 .. Natural (Vectors_In_Order.Length)
           when (for all Element of Vectors_In_Order (Ith)
                 => not Vectors_In_Order (Jth).Contains (Element))
         loop

            declare
               Remainder : Weight_Vector;
               --  contains package weights not in the other two
            begin

               for Each
                 of Package_Weights
                 when not (Vectors_In_Order (Ith).Contains (Each)
                           or else Vectors_In_Order (Jth).Contains (Each))
               loop
                  Remainder.Append (Each);
               end loop;

               --  if we find this one, then the fact we've sorted the list
               --  means we've found the winner!
               if Potential_Distributions.Contains (Remainder) then
                  return Quantum_Entanglement (Vectors_In_Order (Ith));
               end if;

            end;

         end loop;

      end loop;

      raise Not_Found;
   end Part_1;

   --  SUBSECTION
   --  part 2

   function Part_2 return Entanglement_Value is
      Goal_Weight      : constant Entanglement_Value :=
        Package_Weights'Reduce ("+", 0) / 4;
      Division         : Weight_Vector;
      Vectors_In_Order : Positive_Vector_Vector;
   begin

      IO.Put_Line ("Distributing");
      for Ith in 1 .. Package_Weights.Last_Index loop
         Distribute_Weights_From (Ith, Goal_Weight, Division, 0);
      end loop;

      IO.Put_Line ("Sorting");
      for Each of Potential_Distributions loop
         Vectors_In_Order.Append (Each);
      end loop;
      Number_Then_Entanglement.Sort (Vectors_In_Order);

      IO.Put_Line ("Searching");
      --  find the first distribution for which we can find two others
      --  that reach the goal weight
      for Ith in 1 .. Natural (Vectors_In_Order.Length) loop

         --  we have to check for a second
         for Jth
           in Ith + 1 .. Natural (Vectors_In_Order.Length)
           when (for all Element of Vectors_In_Order (Ith)
                 => not Vectors_In_Order (Jth).Contains (Element))
         loop

            --  ...and for a third in this part, but a fourth is forced by 3;
            --  we also have to make sure the first three don't overlap
            for Kth
              in Jth + 1 .. Natural (Vectors_In_Order.Length)
              when (for all Element of Vectors_In_Order (Ith)
                    => not Vectors_In_Order (Kth).Contains (Element))
              and then (for all Element of Vectors_In_Order (Jth)
                        => not Vectors_In_Order (Kth).Contains (Element))
            loop

               declare
                  Remainder : Weight_Vector;
                  --  contains package weights not in the other three
               begin

                  for Each
                    of Package_Weights
                    when not (Vectors_In_Order (Ith).Contains (Each)
                              or else Vectors_In_Order (Jth).Contains (Each)
                              or else Vectors_In_Order (Kth).Contains (Each))
                  loop
                     Remainder.Append (Each);
                  end loop;

                  --  if we find this one, then the fact we've sorted the list
                  --  means we've found the winner!
                  if Potential_Distributions.Contains (Remainder) then
                     return Quantum_Entanglement (Vectors_In_Order (Ith));
                  end if;

               end;

            end loop;

         end loop;

      end loop;

      raise Not_Found;

   end Part_2;

   --  SECTION
   --  main

begin

   Read_Input;

   IO.Put_Line
     ("The quantum entanglement of the first group of packages "
      & "in the ideal configuration is"
      & Part_1'Image);

   Potential_Distributions.Clear;
   IO.Put_Line
     ("The quantum entanglement of the first group of packages "
      & "in the ideal configuration WITH A TRUNK is"
      & Part_2'Image);

end Day24;
