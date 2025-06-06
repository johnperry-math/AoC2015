--  Advent of Code 2015
--  John Perry
--
-- identify your Aunt Sue

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day16 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   --  SECTION
   --  global types and variables

   type Compound_Enum is
     (Children,
      Cats,
      Samoyeds,
      Pomeranians,
      Akitas,
      Vizslas,
      Goldfish,
      Trees,
      Cars,
      Perfumes);

   type Optional_Natural_Record (Known : Boolean := False) is record
      case Known is
         when True =>
            Value : Natural;

         when False =>
            null;
      end case;
   end record;

   type Aunt_Sue_Record is array (Compound_Enum) of Optional_Natural_Record;

   Detected : constant Aunt_Sue_Record :=
     [Children    => Optional_Natural_Record'(Known => True, Value => 3),
      Cats        => Optional_Natural_Record'(Known => True, Value => 7),
      Samoyeds    => Optional_Natural_Record'(Known => True, Value => 2),
      Pomeranians => Optional_Natural_Record'(Known => True, Value => 3),
      Akitas      => Optional_Natural_Record'(Known => True, Value => 0),
      Vizslas     => Optional_Natural_Record'(Known => True, Value => 0),
      Goldfish    => Optional_Natural_Record'(Known => True, Value => 5),
      Trees       => Optional_Natural_Record'(Known => True, Value => 3),
      Cars        => Optional_Natural_Record'(Known => True, Value => 2),
      Perfumes    => Optional_Natural_Record'(Known => True, Value => 1)];

   Aunts_Sue : array (1 .. 500) of Aunt_Sue_Record :=
     [others => [others => Optional_Natural_Record'(Known => False)]];

   --  SECTION
   --  I/O

   package Natural_IO is new IO.Integer_IO (Num => Natural);
   package Compound_IO is new IO.Enumeration_IO (Enum => Compound_Enum);

   procedure Read_Input is
      Input : IO.File_Type;
      Ith   : Positive := 1;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line           : constant String := IO.Get_Line (Input);
            Space_Position : Positive := Strings.Index (Line, " ", Line'First);
            Compound       : Compound_Enum;
            Number         : Natural;
         begin
            Space_Position := Strings.Index (Line, " ", Space_Position + 1);
            Space_Position := @ + 1;
            while Space_Position < Line'Last loop
               Compound_IO.Get
                 (Line (Space_Position .. Line'Last),
                  Compound,
                  Space_Position);
               Natural_IO.Get
                 (Line (Space_Position + 3 .. Line'Last),
                  Number,
                  Space_Position);
               Space_Position := @ + 3;
               Aunts_Sue (Ith) (Compound) :=
                 Optional_Natural_Record'(True, Number);
            end loop;
         end;
         Ith := @ + 1;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   Didnt_Find_Aunt_Sue : exception;

   function Part_1 return Natural is
   begin
      for Ith in Aunts_Sue'Range loop
         declare
            Her : constant Aunt_Sue_Record := Aunts_Sue (Ith);
         begin
            if (for all Compound in Compound_Enum
                => (not Her (Compound).Known)
                   or else Her (Compound).Value = Detected (Compound).Value)
            then
               return Ith;
            end if;
         end;
      end loop;
      raise Didnt_Find_Aunt_Sue with "in Part 1";
   end Part_1;

   function Part_2 return Natural is
   begin
      for Ith in Aunts_Sue'Range loop
         declare
            Her : constant Aunt_Sue_Record := Aunts_Sue (Ith);
         begin
            if (for all Compound in Compound_Enum
                => (case Compound is
                      when Cats | Trees =>
                        (not Her (Compound).Known)
                        or else Her (Compound).Value
                                > Detected (Compound).Value,
                      when Pomeranians | Goldfish =>
                        (not Her (Compound).Known)
                        or else Her (Compound).Value
                                < Detected (Compound).Value,
                      when others =>
                        (not Her (Compound).Known)
                        or else Her (Compound).Value
                                = Detected (Compound).Value))
            then
               return Ith;
            end if;
         end;
      end loop;
      raise Didnt_Find_Aunt_Sue with "in Part 2";
   end Part_2;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line ("I think it's Aunt Sue" & Part_1'Image);
   IO.Put_Line ("No, it's Aunt Sue" & Part_2'Image);
end Day16;
