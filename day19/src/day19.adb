--  Advent of Code 2015
--  John Perry
--
--  day 19: red-nosed reindeer medicine

pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day19 is

   package IO renames Ada.Text_IO;
   use all type Ada.Containers.Count_Type;
   use all type Ada.Containers.Hash_Type;
   subtype Hash_Type is Ada.Containers.Hash_Type;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  types first

   --  SUBSUBSECTION
   --  Element_String: a two-character string
   --  to keep Element_String consistent and make our lives easier,
   --  we allow the second element to be ' ' when the element
   --  would otherwise be just one character

   type Element_String is new String (1 .. 2);

   function Hash (Element : Element_String) return Hash_Type
   is (Hash_Type
         (Character'Pos (Element (1)) * 128 + Character'Pos (Element (2))));

   --  SUBSUBSECTION
   --  Element_Vectors: a sequence of Element_String

   package Element_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Element_String);
   subtype Element_Vector is Element_Vectors.Vector;

   function "=" (Left, Right : Element_Vector) return Boolean
   is (Left.Length = Right.Length
       and then (for all Ith in Left.First_Index .. Left.Last_Index
                 => Left (Ith) = Right (Ith)));

   function Hash (Sequence : Element_Vector) return Hash_Type is
      type Mod_32003 is mod 32003;
      Result : Mod_32003 := 0;
   begin
      for Element of Sequence loop
         Result :=
           @
           + Mod_32003
               (Character'Pos (Element (1))
                * 128
                + Character'Pos (Element (2)));
      end loop;
      return Hash_Type (Result);
   end Hash;

   --  SUBSUBSECTION
   --  Rewrite_Vectors: a sequence of Element_Vector that define
   --  how to rewrite an Element_String; see also Rewrite_Maps

   package Rewrite_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Element_Vector);
   subtype Rewrite_Vector is Rewrite_Vectors.Vector;

   function "=" (Left, Right : Rewrite_Vector) return Boolean
   is (Left.Length = Right.Length
       and then (for all Ith in Left.First_Index .. Left.Last_Index
                 => Left (Ith) = Right (Ith)));

   --  SUBSUBSECTION
   --  Rewrite_Maps: a mapping of Element_String to Rewrite_Vector

   package Rewrite_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Element_String,
        Element_Type    => Rewrite_Vector,
        Hash            => Hash,
        Equivalent_Keys => "=");
   subtype Rewrite_Map is Rewrite_Maps.Map;

   --  SUBSUBSECTION
   --  Compound_Sets: a set of Element_Vector

   package Compound_Sets is new
     Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => Element_Vector,
        Hash                => Hash,
        Equivalent_Elements => "=");
   subtype Compound_Set is Compound_Sets.Set;

   --  SUBSECTION
   --  variables

   Rules    : Rewrite_Map;
   Medicine : Element_Vector;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  input processing

   function Next_Element
     (Sequence : String; Position : Positive) return Element_String
   is
      --  returns the Element in Sequence that starts at Position
      Result : Element_String := "  ";
   begin
      Result (1) := Sequence (Position);
      --  check for a two-character sequence (capital, then miniscule)
      if Position + 1 <= Sequence'Last
        and then Sequence (Position + 1) in 'a' .. 'z'
      then
         Result (2) := Sequence (Position + 1);
      end if;
      return Result;
   end Next_Element;

   function To_Element_Sequence
     (Line : String; Start : Positive) return Element_Vector
   is
      --  converts Line to an Element_Vector, starting from position Start
      Result   : Element_Vector;
      Position : Positive := Start;
   begin
      while Position <= Line'Last loop
         declare
            Target : constant Element_String := Next_Element (Line, Position);
         begin
            Result.Append (Target);
            --  check for a one-character element (second character is space)
            Position := (if Target (2) = ' ' then @ + 1 else @ + 2);
         end;
      end loop;
      return Result;
   end To_Element_Sequence;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            exit when Line'Length = 0;
            declare
               Source    : constant Element_String :=
                 Element_String (Line (1 .. 2));
               Rewriting : constant Element_Vector :=
                 To_Element_Sequence
                   (Line, (if Source (2) = ' ' then 6 else 7));
            begin
               if not Rules.Contains (Source) then
                  declare
                     Empty_Rule : Rewrite_Vector;
                  begin
                     Rules.Insert (Source, Empty_Rule);
                  end;
               end if;
               declare
                  Rule : constant Rewrite_Maps.Reference_Type :=
                    Rules.Reference (Source);
               begin
                  Rule.Append (Rewriting);
               end;
            end;
         end;
      end loop;
      declare
         Line : constant String := IO.Get_Line (Input);
      begin
         Medicine := To_Element_Sequence (Line, Line'First);
      end;
      IO.Close (Input);
   end Read_Input;

   --  SUBSECTION
   --  output

   procedure Put (Element : Element_String) is
      --  writes Element to standard output. if it is a one-character element,
      --  we do not write the space
   begin
      IO.Put (Element (1));
      if Element (2) /= ' ' then
         IO.Put (Element (2));
      end if;
   end Put;

   procedure Put (Sequence : Element_Vector) is
      --  writes Sequence to standard output
   begin
      for Each of Sequence loop
         Put (Each);
      end loop;
   end Put;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  part 1

   function Options_Of (Compound : Element_Vector) return Compound_Set is
      --  returns all the compounds that can be made from Compound.
      --  this was originally in Part_1, but i rather foolishly thought that
      --  going "forward" was the right idea for part 2, so
      --  i thought i'd be "smart" and reuse this...
      Options : Compound_Set;
   begin
      for Ith in 1 .. Compound.Last_Index loop
         declare
            Source : constant Element_String := Compound (Ith);
         begin
            if Rules.Contains (Source) then
               for Target of Rules (Source) loop
                  declare
                     New_Rule : Element_Vector;
                  begin
                     for Jth in 1 .. Ith - 1 loop
                        New_Rule.Append (Compound (Jth));
                     end loop;
                     for Each of Target loop
                        New_Rule.Append (Each);
                     end loop;
                     for Jth in Ith + 1 .. Compound.Last_Index loop
                        New_Rule.Append (Compound (Jth));
                     end loop;
                     if not Options.Contains (New_Rule) then
                        Options.Insert (New_Rule);
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
      return Options;
   end Options_Of;

   function Part_1 return Natural is
      Options : constant Compound_Set := Options_Of (Medicine);
   begin
      return Natural (Options.Length);
   end Part_1;

   --  SUBSECTION
   --  part 2

   function Part_2 return Natural is
      --  reduces Medicine repeatedly until it matches "e ";
      --  on each iteration, it looks for the largest matching sequence
      --  and replaces it by the generator

      Steps   : Natural := 0;
      Current : Element_Vector := Medicine;
      Target  : constant Element_Vector := [Element_String'("e ")];

      Debugging : constant Boolean := False;

   begin

      while Current /= Target loop

         Steps := @ + 1;

         declare
            Winning_Rewrite            : Element_String;
            --  the longest rewritable subsequence of Current
            Winning_Position           : Positive;
            --  where we find Winning_Rewrite
            Longest_Replaceable_Length : Ada.Containers.Count_Type := 0;
            --  length of Winning_Rewrite
            Position                   : Positive := Current.First_Index;
            --  present location in Current
            Next                       : Element_Vector;
            --  target sequence
         begin

            --  first find the longest rewritable susbequence of Current
            while Position <= Current.Last_Index loop
               for Cursor in Rules.Iterate loop
                  for Rewriting of Rewrite_Maps.Element (Cursor) loop

                     --  don't waste time with losers
                     --  don't waste time on rules that are too long
                     --  only waste time to ensure that they match
                     if Rewriting.Length > Longest_Replaceable_Length
                       and then Natural (Rewriting.Length)
                                <= Current.Last_Index - Position + 1
                       and then (for all Ith in 1 .. Rewriting.Last_Index
                                 => Rewriting (Ith)
                                    = Current (Position + Ith - 1))
                     then
                        Winning_Position := Position;
                        Longest_Replaceable_Length := Rewriting.Length;
                        Winning_Rewrite := Rewrite_Maps.Key (Cursor);
                     end if;

                  end loop;
               end loop;
               Position := @ + 1;
            end loop;

            --  i'll leave this debugging code in for entertainment purposes
            if Debugging then
               IO.Put (Current.Length'Image & " ");
               Put (Current);
               IO.New_Line;
               IO.Put_Line ("Winning Position is" & Winning_Position'Image);
               IO.Put ("Rewriting by ");
               Put (Winning_Rewrite);
               IO.New_Line;
               IO.Put_Line
                 ("Longest replaceable length"
                  & Longest_Replaceable_Length'Image);
            end if;

            --  generate the reduction
            for Ith in 1 .. Winning_Position - 1 loop
               Next.Append (Current (Ith));
            end loop;
            Next.Append (Winning_Rewrite);
            for Ith
              in Winning_Position
                 + Natural (Longest_Replaceable_Length)
                 .. Current.Last_Index
            loop
               Next.Append (Current (Ith));
            end loop;
            Current := Next;
         end;
      end loop;

      return Steps;

   end Part_2;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line ("There are " & Part_1'Image & " distinct molecules");
   IO.Put_Line
     ("It would take" & Part_2'Image & " steps to create the medicine");
end Day19;
