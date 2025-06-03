--  Advent of Code 2015
--  John Perry
--
--  arrange your family feast so as to maximize the diners' happiness

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day13 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   package Strings renames Ada.Strings.Fixed;

   --  SECTION
   --  global types and variables

   type Family_Enum is
     (Alice, Bob, Carol, David, Eric, Frank, George, Mallory, You);
   --  part 2 needs You

   Happiness_Matrix : array (Family_Enum, Family_Enum) of Integer :=
     [others => [others => 0]];
   --  default is 0 especially so that we don't have to reassign You

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for First in Family_Enum when First /= You loop
         for Second in Family_Enum when First /= Second and then Second /= You
         loop
            declare
               Line         : constant String := IO.Get_Line (Input);
               First_Space  : constant Positive :=
                 Strings.Index (Line, " ", Line'First);
               Second_Space : constant Positive :=
                 Strings.Index (Line, " ", First_Space + 1);
               Third_Space  : Positive :=
                 Strings.Index (Line, " ", Second_Space + 1);
            begin
               Positive_IO.Get
                 (Line (Third_Space + 1 .. Line'Last),
                  Happiness_Matrix (First, Second),
                  Third_Space);
               if Line (Second_Space + 1 .. Second_Space + 4) = "lose" then
                  Happiness_Matrix (First, Second) := @ * (-1);
               end if;
            end;
         end loop;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   type Part_Enum is (First, Second);

   subtype Seat_Range is
     Positive
       range 1
             .. (Family_Enum'Pos (Family_Enum'Last)
                 - Family_Enum'Pos (Family_Enum'First)
                 + 1);
   --  sigh

   type Dining_Arrangement is array (Seat_Range) of Family_Enum;

   function Max_Happiness_Of
     (Arrangement  : Dining_Arrangement;
      Num_Assigned : Seat_Range;
      Part         : Part_Enum) return Integer
   is
      --  this recursively arranges the table or scores it,
      -- depending on whether we are in a final case
      Result : Integer;
      Limit  : Seat_Range :=
        (if Part = First then Arrangement'Length - 1 else Arrangement'Length);
      --  in part 1, the limit should be 8; otherwise, it's 9
   begin
      if Num_Assigned = Limit then
         --  we have arranged the table; now need to tally happiness
         Result := 0;
         for Ith in Arrangement'First .. Limit - 1 loop
            Result :=
              @
              + Happiness_Matrix (Arrangement (Ith), Arrangement (Ith + 1))
              + Happiness_Matrix (Arrangement (Ith + 1), Arrangement (Ith));
         end loop;
         Result :=
           @
           + Happiness_Matrix (Arrangement (1), Arrangement (Limit))
           + Happiness_Matrix (Arrangement (Limit), Arrangement (1));
      else
         --  we need to add more diners
         declare
            New_Arrangement : Dining_Arrangement := Arrangement;
         begin
            Result := Integer'First;
            for Member
              in Family_Enum
              when (for all Ith in 1 .. Num_Assigned
                    => Arrangement (Ith) /= Member)
              and then (Part = Second or else Member /= You)
            loop
               New_Arrangement (Num_Assigned + 1) := Member;
               Result :=
                 Integer'Max
                   (Result,
                    Max_Happiness_Of
                      (New_Arrangement, Num_Assigned + 1, Part));
            end loop;
         end;
      end if;
      return Result;
   end Max_Happiness_Of;

   function Solution (Part : Part_Enum) return Integer is
      --  solves both parts by calling Max_Happiness_Of
      --  on each member of Family_Enum -- though in Part 1 we ignore You
      Result      : Integer := Integer'First;
      Arrangement : Dining_Arrangement;
   begin
      for Member in Family_Enum loop
         if Part = Second or else Member /= You then
            Arrangement (1) := Member;
            Result :=
              Integer'Max (Result, Max_Happiness_Of (Arrangement, 1, Part));
         end if;
      end loop;
      return Result;
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("The total change in happiness for the optimal seating arrangement is"
      & Solution (First)'Image);
   IO.Put_Line ("When including you, it becomes" & Solution (Second)'Image);
end Day13;
