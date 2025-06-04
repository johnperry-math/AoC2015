--  Advent of Code 2015
--  John Perry
--
--  help Santa determine the winner of a reindeer race

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day14 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   package Positive_IO is new IO.Integer_IO (Num => Positive);

   --  SECTION
   --  global types and variables

   type Reindeer_Enum is
     (Rudolph, Cupid, Prancer, Donner, Dasher, Comet, Blitzen, Vixen, Dancer);

   type Reindeer_Info_Record is record
      Air_Speed, Flying_Time, Resting_Time : Positive;
   end record;

   Reindeer_Info : array (Reindeer_Enum) of Reindeer_Info_Record;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for Reindeer in Reindeer_Enum loop
         declare
            Line                                 : constant String :=
              IO.Get_Line (Input);
            Space_Index                          : Positive := Line'First;
            Air_Speed, Flying_Time, Resting_Time : Positive;
         begin
            for Each in 1 .. 3 loop
               Space_Index := Strings.Index (Line, " ", Space_Index);
               Space_Index := @ + 1;
            end loop;
            Positive_IO.Get
              (Line (Space_Index .. Line'Last), Air_Speed, Space_Index);
            for Each in 1 .. 3 loop
               Space_Index := Strings.Index (Line, " ", Space_Index);
               Space_Index := @ + 1;
            end loop;
            Positive_IO.Get
              (Line (Space_Index .. Line'Last), Flying_Time, Space_Index);
            for Each in 1 .. 7 loop
               Space_Index := Strings.Index (Line, " ", Space_Index);
               Space_Index := @ + 1;
            end loop;
            Positive_IO.Get
              (Line (Space_Index .. Line'Last), Resting_Time, Space_Index);
            Reindeer_Info (Reindeer) := (Air_Speed, Flying_Time, Resting_Time);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  types and variables for solution

   type Status_Enum is (Flying, Resting);

   type Status_Record is record
      Activity       : Status_Enum;
      Time_Remaining : Natural;
   end record;

   Time_Limit : constant Positive := 2_503;

   type Part_Enum is (First, Second);

   --  SUBSECTION
   --  implementation

   function Solution (Part : Part_Enum) return Positive is
      --  solves parts 1 and 2!
      Statuses : array (Reindeer_Enum) of Status_Record :=
        [for Reindeer in Reindeer_Enum
         => (Flying, Reindeer_Info (Reindeer).Flying_Time)];
      --  initialize reindeer as flying

      Result   : Positive := 1;
      Distance : array (Reindeer_Enum) of Natural := [others => 0];
      --  scoring for part 1
      Points   : array (Reindeer_Enum) of Natural := [others => 0];
      --  scoring for part 2

      Lead_Distance : Natural := 0;
   begin
      for Each in 1 .. Time_Limit loop
         --  adjust all reindeer by 1 second
         for Reindeer in Reindeer_Enum loop
            Statuses (Reindeer).Time_Remaining := @ - 1;
            if Statuses (Reindeer).Activity = Flying then
               Distance (Reindeer) := @ + Reindeer_Info (Reindeer).Air_Speed;
            end if;
            Lead_Distance := Natural'Max (Lead_Distance, Distance (Reindeer));
            if Statuses (Reindeer).Time_Remaining = 0 then
               case Statuses (Reindeer).Activity is
                  when Flying =>
                     Statuses (Reindeer) :=
                       (Resting, Reindeer_Info (Reindeer).Resting_Time);

                  when Resting =>
                     Statuses (Reindeer) :=
                       (Flying, Reindeer_Info (Reindeer).Flying_Time);
               end case;
            end if;
         end loop;
         --  for part 2
         for Reindeer in Reindeer_Enum loop
            if Distance (Reindeer) = Lead_Distance then
               Points (Reindeer) := @ + 1;
            end if;
         end loop;
      end loop;
      --  determine winner
      for Reindeer in Reindeer_Enum loop
         Result :=
           Positive'Max
             (Result,
              (if Part = First
               then Distance (Reindeer)
               else Points (Reindeer)));
      end loop;
      return Result;
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("After"
      & Time_Limit'Image
      & " seconds, the furthest a reindeer flew was"
      & Solution (First)'Image);
   IO.Put_Line
     ("The winning reindeer has" & Solution (Second)'Image & " points");
end Day14;
