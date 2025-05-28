--  Advent of Code 2015
--  John Perry
--
--  determine the differences in memory size
--  between formatted and unformatted strings

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Indefinite_Vectors;

procedure Day08 is
   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   package String_Vecs is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => String);

   Raw_Strings : String_Vecs.Vector;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            Raw_Strings.Append (Line);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   Invalid_Control : exception;

   --  SECTION
   --  part 1

   function Formatted_Length (Value : String) return Natural is
      --  returns the difference between Value and the result of applying
      --  its embedded formatting. the only recognized formatting is
      --
      --  | raw value  |  formatted value   |
      --  |------------|--------------------|
      --  |     \\     |         \          |
      --  |     \"     |         "          |
      --  |    \xab    | character w/hex ab |
      --  | aught else |       itself       |
      --
      Result   : Natural := 0;
      Position : Positive := Value'First + 1;
   begin
      while Position < Value'Last loop
         --  we always add at least 1
         Result := @ + 1;

         --  determine where next formatted character lies in raw string
         if Value (Position) = '\' then
            --  control characters skip more than 1
            case Value (Position + 1) is
               when '\' | '"' =>
                  --  skip for \ and the matching character
                  Position := @ + 2;

               when 'x' =>
                  --  we expect two hex's after x, so skip 4 altogether
                  Position := @ + 4;

               when others =>
                  raise Invalid_Control
                    with "at" & Position'Image & " " & Value;
            end case;
         else
            --  not a control character so don't skip
            Position := @ + 1;
         end if;
      end loop;
      return Result;
   end Formatted_Length;

   function Part_1 return Natural is
      Result : Natural := 2;
   begin
      for Raw_String of Raw_Strings loop
         Result := @ + Raw_String'Length - Formatted_Length (Raw_String);
      end loop;
      return Result;
   end Part_1;

   --  SECTION
   --  part 2

   function Unformatted_Length (Value : String) return Natural is
      --  returns the length of a raw string that, when formatted,
      --  gives us Value
      --
      --  this essentially reverses the logic of Formatted_Length,
      --  yet oddly it turns out to be much simpler:
      --  * we need at least 2 for the quotation marks to start and end it;
      --  * a quote or a backslash needs a backslash to precede it,
      --    so it adds 2 to the length; and
      --  * anything else goes straight through, so it adds 1 to the length
      Result : Natural := 2;
   begin
      for Char of Value loop
         Result :=
           @
           + (case Char is
                when '"' | '\' => 2,
                when others => 1);
      end loop;
      return Result;
   end Unformatted_Length;

   function Part_2 return Natural is
      Result : Natural := 0;
   begin
      for Raw_String of Raw_Strings loop
         Result := @ + Unformatted_Length (Raw_String) - Raw_String'Length;
      end loop;
      return Result;
   end Part_2;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line ("The difference between raw and formatted is" & Part_1'Image);
   IO.Put_Line
     ("The difference between raw and unformatted is" & Part_2'Image);
end Day08;
