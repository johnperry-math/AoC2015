--  Advent of Code 2015
--  John Perry
--
--  count the number of lights lit

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day06 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   type Action_Enum is (Turn_On, Turn_Off, Toggle);

   Invalid_Action : exception;

   subtype Valid_Range is Natural range 0 .. 999;

   type Location_Record is record
      Row, Col : Valid_Range;
   end record;

   type Instruction_Record (Action : Action_Enum := Toggle) is record
      Start, Stop : Location_Record;
   end record;

   package Instruction_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Instruction_Record);

   Instructions : Instruction_Vectors.Vector;

   type Bulb_Enum is (Lit, Unlit);

   --  SECTION: I/O

   function Decode_Action (Line : String) return Action_Enum
   is (if Line (Line'First .. Line'First + 6) = "turn on"
       then Turn_On
       elsif Line (Line'First .. Line'First + 7) = "turn off"
       then Turn_Off
       elsif Line (Line'First .. Line'First + 5) = "toggle"
       then Toggle
       else raise Invalid_Action);

   function Decode_Location
     (Line : String; Position : Positive) return Location_Record
   is
      Row, Col       : Valid_Range;
      Comma_Position : Positive := Position + 1;
      End_Position   : Positive;
   begin
      while Line (Comma_Position) /= ',' loop
         Comma_Position := @ + 1;
      end loop;
      Row := Valid_Range'Value (Line (Position .. Comma_Position - 1));
      End_Position := Comma_Position + 1;
      while End_Position < Line'Last and then Line (End_Position) /= ' ' loop
         End_Position := @ + 1;
      end loop;
      Col := Valid_Range'Value (Line (Comma_Position + 1 .. End_Position));
      return Location_Record'(Row, Col);
   end Decode_Location;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line           : constant String := IO.Get_Line (Input);
            Action         : constant Action_Enum := Decode_Action (Line);
            Start_Position : constant Positive :=
              (case Action is
                 when Turn_On => 9,
                 when Turn_Off => 10,
                 when Toggle => 8);
            Start          : constant Location_Record :=
              Decode_Location (Line, Start_Position);
            Stop_Position  : constant Positive :=
              Start_Position
              + 7 --  through
              + 2 --  spaces
              + 1 --  comma
              + (if Start.Row in 0 .. 9
                 then 1
                 elsif Start.Row in 10 .. 99
                 then 2
                 else 3)
              + (if Start.Col in 0 .. 9
                 then 1
                 elsif Start.Col in 10 .. 99
                 then 2
                 else 3);
            Stop           : constant Location_Record :=
              Decode_Location (Line, Stop_Position);
         begin
            Instructions.Append (Instruction_Record'(Action, Start, Stop));
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   function Part_1 return Natural is
      Grid   : array (Valid_Range, Valid_Range) of Bulb_Enum :=
        [others => [others => Unlit]];
      Result : Natural := 0;
   begin
      for Instruction of Instructions loop
         for Row in Instruction.Start.Row .. Instruction.Stop.Row loop
            for Col in Instruction.Start.Col .. Instruction.Stop.Col loop
               case Instruction.Action is
                  when Turn_On | Turn_Off =>
                     Grid (Row, Col) :=
                       (if Instruction.Action = Turn_On then Lit else Unlit);

                  when Toggle =>
                     Grid (Row, Col) :=
                       (if Grid (Row, Col) = Lit then Unlit else Lit);
               end case;
            end loop;
         end loop;
      end loop;
      for Row in Valid_Range loop
         for Col in Valid_Range loop
            if Grid (Row, Col) = Lit then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Part_1;

   function Part_2 return Natural is
      Grid   : array (Valid_Range, Valid_Range) of Natural :=
        [others => [others => 0]];
      Result : Natural := 0;
   begin
      for Instruction of Instructions loop
         for Row in Instruction.Start.Row .. Instruction.Stop.Row loop
            for Col in Instruction.Start.Col .. Instruction.Stop.Col loop
               case Instruction.Action is
                  when Turn_On | Turn_Off =>
                     Grid (Row, Col) :=
                       (if Instruction.Action = Turn_On
                        then @ + 1
                        else Integer'Max (0, @ - 1));

                  when Toggle =>
                     Grid (Row, Col) := @ + 2;
               end case;
            end loop;
         end loop;
      end loop;
      for Row in Valid_Range loop
         for Col in Valid_Range loop
            Result := @ + Grid (Row, Col);
         end loop;
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("After following the instructions," & Part_1'Image & " lights are lit");
   IO.Put_Line
     ("After correctly following the instructions, the brightness is"
      & Part_2'Image);
end Day06;
