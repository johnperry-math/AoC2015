--  Advent of Code 2015
--  John Perry
--
--  work out the values of a network

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Characters.Handling;

with Interfaces;

procedure Day07 is

   package IO renames Ada.Text_IO;
   package Char renames Ada.Characters.Handling;

   --  SECTION
   --  global types and variables

   subtype Value_Type is Interfaces.Unsigned_64;
   use all type Value_Type;

   subtype Name_String is String (1 .. 2);

   type Input_Type_Enum is (Value, Gate);

   type Input_Record (Kind : Input_Type_Enum := Value) is record
      case Kind is
         when Value =>
            Value : Value_Type;

         when Gate =>
            Feed : Name_String;
      end case;
   end record;

   type Gate_Enum is
     (Value, And_Gate, Left_Shift, Not_Gate, Or_Gate, Right_Shift);

   type Binary_Gate_Record is record
      Left, Right : Input_Record;
   end record;

   type Feed_Record (Gate : Gate_Enum := Value) is record
      case Gate is
         when Value | Not_Gate =>
            Input : Input_Record;

         when And_Gate | Or_Gate | Left_Shift | Right_Shift =>
            Inputs : Binary_Gate_Record;

      end case;
   end record;

   subtype Name_Range is Character range '`' .. 'z';
   --  this hack makes it easy to use a _relatively_ small array
   --  as a mapping of names to gates or values

   type Wire_Array is array (Name_Range, Name_Range) of Feed_Record;

   Wires : Wire_Array;

   --  SECTION
   --  I/O

   function Read_Operand
     (Line : String; Start, Stop : Positive) return Input_Record
   is
      --  reads the operand in the indicated position of Line
      --  and returns an appropriate input record
      --
      --  the convention for a one-character name is '`x'
      --  rather than 'x`', and spaces are unfortunately
      --  not possible with this approach
      Name          : Name_String;
      Operand_Value : Value_Type;
   begin
      if Char.Is_Digit (Line (Start)) then
         Operand_Value := Value_Type'Value (Line (Start .. Stop));
         return Input_Record'(Kind => Value, Value => Operand_Value);
      else
         Name := "``";
         if Stop = Start then
            Name (2) := Line (Start);
         else
            Name := Line (Start .. Start + 1);
         end if;
         return Input_Record'(Kind => Gate, Feed => Name);
      end if;
   end Read_Operand;

   function Is_Direct_Feed (Line : String) return Boolean is
      --  return True if and only if this has the form
      --      first -> second
      --  i.e., no operands
      Position : Positive := Line'First;
   begin
      while Line (Position) /= ' ' loop
         Position := @ + 1;
      end loop;
      return Line (Position + 1) = '-';
   end Is_Direct_Feed;

   function Read_Direct_Feed (Line : String) return Feed_Record is
      --  reads a direct feed (see Is_Direct_Feed) and returns
      --  a corresponding record
      Position : Positive := Line'First;
   begin
      while Line (Position) /= ' ' loop
         Position := @ + 1;
      end loop;
      return
        Feed_Record'
          (Gate  => Value,
           Input => Read_Operand (Line, Line'First, Position - 1));
   end Read_Direct_Feed;

   function Read_Not_Feed (Line : String) return Feed_Record
   is (Feed_Record'
         (Gate  => Not_Gate,
          Input =>
            Input_Record'
              (Kind => Gate,
               Feed =>
                 (if Line (Line'First + 5) = ' '
                  then "`" & Line (Line'First + 4)
                  else Line (Line'First + 4 .. Line'First + 5)))));

   Invalid_Gate : exception;
   --  this proved very useful when debugging

   function Read_Binary_Feed (Line : String) return Feed_Record is
      --  this will fail if Line does not hold a binary operator
      --  (AND, OR, LSHIFT, RSHIFT)
      Start_Read, Stop_Read : Positive := Line'First;
      First_Op, Second_Op   : Input_Record;
      Gate                  : Gate_Enum;
   begin
      --  find and read the first operand
      while Line (Stop_Read) /= ' ' loop
         Stop_Read := @ + 1;
      end loop;
      First_Op := Read_Operand (Line, Start_Read, Stop_Read - 1);

      --  determine the operator
      case Line (Stop_Read + 1) is
         when 'A' =>
            --  AND
            Gate := And_Gate;
            Start_Read := Stop_Read + 5;

         when 'O' =>
            --  OR
            Gate := Or_Gate;
            Start_Read := Stop_Read + 4;

         when 'L' =>
            --  LSHIFT
            Gate := Left_Shift;
            Start_Read := Stop_Read + 8;

         when 'R' =>
            --  RSHIFT
            Gate := Right_Shift;
            Start_Read := Stop_Read + 8;

         when others =>
            --  not binary; how are we here?!?
            raise Invalid_Gate with Line;
      end case;

      --  find and read the second operator
      Stop_Read := Start_Read;
      while Line (Stop_Read) /= ' ' loop
         Stop_Read := @ + 1;
      end loop;
      Second_Op := Read_Operand (Line, Start_Read, Stop_Read - 1);

      return
        (case Gate is
           when And_Gate =>
             Feed_Record'
               (And_Gate,
                Inputs =>
                  Binary_Gate_Record'(Left => First_Op, Right => Second_Op)),
           when Or_Gate =>
             Feed_Record'
               (Or_Gate,
                Inputs =>
                  Binary_Gate_Record'(Left => First_Op, Right => Second_Op)),
           when Left_Shift =>
             Feed_Record'
               (Left_Shift,
                Inputs =>
                  Binary_Gate_Record'(Left => First_Op, Right => Second_Op)),
           when Right_Shift =>
             Feed_Record'
               (Right_Shift,
                Inputs =>
                  Binary_Gate_Record'(Left => First_Op, Right => Second_Op)),
           when others => raise Invalid_Gate with Line);
   end Read_Binary_Feed;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Out_Name : Name_String := Line (Line'Last - 1 .. Line'Last);
            Feed     : constant Feed_Record :=
              (if Line (Line'First) = 'N'
               then Read_Not_Feed (Line)
               elsif Is_Direct_Feed (Line)
               then Read_Direct_Feed (Line)
               else Read_Binary_Feed (Line));
         begin
            if Out_Name (1) = ' ' then
               Out_Name (1) := '`';
            end if;
            Wires (Out_Name (1), Out_Name (2)) := Feed;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  memoizing wire values

   type Maybe_Value (Holds_Value : Boolean := False) is record
      case Holds_Value is
         when True =>
            Value : Value_Type;

         when False =>
            null;
      end case;
   end record;

   Wire_Values : array (Name_Range, Name_Range) of Maybe_Value;

   --  SUBSECTION
   --  recursive evaluation

   function Evaluate (Input : Input_Record) return Value_Type is
   begin
      --  if the value is immediate, return it
      if Input.Kind = Value then
         return Input.Value;
      end if;

      --  not immediate but named; extract the name and evalute recursively
      declare
         Name       : constant Name_String := Input.Feed;
         Wire renames Wires (Name (1), Name (2));
         Name_Value : Value_Type;
      begin
         --  if we already know the value, return it
         if Wire_Values (Name (1), Name (2)).Holds_Value then
            return Wire_Values (Name (1), Name (2)).Value;
         end if;
         --  we don't know the value, so evaluate it recursively
         Name_Value :=
           (case Wire.Gate is
              when Value => Evaluate (Wire.Input),

              when Not_Gate => not Evaluate (Wire.Input),

              when And_Gate =>
                Evaluate (Wire.Inputs.Left) and Evaluate (Wire.Inputs.Right),

              when Or_Gate =>
                Evaluate (Wire.Inputs.Left) or Evaluate (Wire.Inputs.Right),

              when Left_Shift =>
                Interfaces.Shift_Left
                  (Evaluate (Wire.Inputs.Left),
                   Natural (Evaluate (Wire.Inputs.Right))),

              when Right_Shift =>
                Interfaces.Shift_Right
                  (Evaluate (Wire.Inputs.Left),
                   Positive (Evaluate (Wire.Inputs.Right))));
         --  record it in the memo before returning
         Wire_Values (Name (1), Name (2)) :=
           (Holds_Value => True, Value => Name_Value);
         return Name_Value;
      end;
   end Evaluate;

   --  SUBSECTION
   --  part 1

   function Part_1 return Value_Type is
   begin
      return Evaluate (Input_Record'(Gate, "`a"));
   end Part_1;

   --  SUBSSECTION
   --  part 2

   function Part_2 (Value_Of_A : Value_Type) return Value_Type is
   begin
      --  reset wires, then set b to a's value
      Wire_Values :=
        [others => [others => Maybe_Value'(Holds_Value => False)]];
      Wire_Values ('`', 'b') :=
        Maybe_Value'(Holds_Value => True, Value => Value_Of_A);

      return Evaluate (Input_Record'(Gate, "`a"));
   end Part_2;

   --  SECTION
   --  main

begin
   Read_Input;
   declare
      Value_Of_A : Value_Type := Part_1;
   begin
      IO.Put_Line ("the value of a is" & Value_Of_A'Image);
      IO.Put_Line
        ("after setting b to that and resetting others, the value of a is"
         & Part_2 (Value_Of_A)'Image);
   end;
end Day07;
