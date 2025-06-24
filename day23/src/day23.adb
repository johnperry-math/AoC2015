--  Advent of Code 2015
--  John Perry
--
--  help a child understand what her new toy computer does

pragma Ada_2022;

with Ada.Text_IO;

procedure Day23 is

   package IO renames Ada.Text_IO;
   package Integer_IO is new IO.Integer_IO (Num => Integer);

   Debugging : constant Boolean := False;

   --  SECTION
   --  global types, variables, and generic instantiations

   --  SUBSECTION
   --  registers

   type Register_Enum is (A, B);

   package Register_IO is new IO.Enumeration_IO (Enum => Register_Enum);

   type Register_Array is array (Register_Enum) of Natural;

   --  SUBSECTION
   --  instructions

   type Instruction_Enum is (Hlf, Tpl, Inc, Jmp, Jie, Jio, Stop);

   package Instruction_IO is new IO.Enumeration_IO (Enum => Instruction_Enum);

   type Instruction_Record (Kind : Instruction_Enum := Stop) is record
      case Kind is
         when Hlf | Tpl | Inc =>
            Register : Register_Enum;

         when Jmp | Jie | Jio =>
            Offset : Integer;
            case Kind is

               when Jie | Jio =>
                  Test_Register : Register_Enum;

               when others =>
                  null;
            end case;

         when Stop =>
            null;
      end case;
   end record;

   --  SUBSECTION
   --  memory

   subtype Memory_Range is Positive range 1 .. 1_000;
   --  1_000 turned out to be overkill...

   type Memory_Array is array (Memory_Range) of Instruction_Record;

   Memory_Start : Memory_Array :=
     [others => Instruction_Record'(Kind => Stop)];

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  input

   procedure Read_Input is
      Input               : IO.File_Type;
      Instruction_Pointer : Memory_Range := Memory_Range'First;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line        : constant String := IO.Get_Line (Input);
            Position    : Positive;
            Instruction : Instruction_Enum;
            Register    : Register_Enum;
            Offset      : Integer;
         begin
            Instruction_IO.Get (Line, Instruction, Position);
            if Instruction in Hlf .. Inc or else Instruction in Jie .. Jio then
               Register_IO.Get
                 (Line (Position + 2 .. Line'Last), Register, Position);
            end if;
            if Instruction = Jmp then
               Integer_IO.Get
                 (Line (Position + 2 .. Line'Last), Offset, Position);
            end if;
            if Instruction in Jie .. Jio then
               Integer_IO.Get
                 (Line (Position + 3 .. Line'Last), Offset, Position);
            end if;
            case Instruction is
               when Hlf =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'(Kind => Hlf, Register => Register);

               when Tpl =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'(Kind => Tpl, Register => Register);

               when Inc =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'(Kind => Inc, Register => Register);

               when Jmp =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'(Kind => Jmp, Offset => Offset);

               when Jie =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'
                      (Kind          => Jie,
                       Test_Register => Register,
                       Offset        => Offset);

               when Jio =>
                  Memory_Start (Instruction_Pointer) :=
                    Instruction_Record'
                      (Kind          => Jio,
                       Test_Register => Register,
                       Offset        => Offset);

               when Stop =>
                  null;
            end case;
         end;
         Instruction_Pointer := @ + 1;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SUBSECTION
   --  output

   procedure Put (Instruction_Pointer : Memory_Range; Memory : Memory_Array) is
      Instruction : constant Instruction_Record :=
        Memory (Instruction_Pointer);
   begin
      IO.Put (Instruction_Pointer'Image & ": ");
      Instruction_IO.Put (Instruction.Kind);
      IO.Put (' ');
      case Instruction.Kind is
         when Hlf | Tpl | Inc =>
            Register_IO.Put (Instruction.Register);

         when Jmp | Jie | Jio =>
            case Instruction.Kind is
               when Jmp =>
                  null;

               when Jie | Jio =>
                  Register_IO.Put (Instruction.Test_Register, 0);
                  IO.Put (' ');

               when others =>
                  null;
            end case;
            Integer_IO.Put (Instruction.Offset, 0);

         when Stop =>
            null;
      end case;
   end Put;

   --  SECTION
   --  parts 1 and 2

   End_Of_Program : exception;

   procedure Interpret
     (Instruction_Pointer : in out Memory_Range;
      Memory              : Memory_Array;
      Registers           : in out Register_Array)
   is
      Instruction : constant Instruction_Record :=
        Memory (Instruction_Pointer);
   begin
      if Debugging then
         Put (Instruction_Pointer, Memory);
         IO.New_Line;
         IO.Put_Line (Registers (A)'Image & Registers (B)'Image);
      end if;
      case Instruction.Kind is
         when Hlf =>
            Registers (Instruction.Register) := @ / 2;
            Instruction_Pointer := @ + 1;

         when Tpl =>
            Registers (Instruction.Register) := @ * 3;
            Instruction_Pointer := @ + 1;

         when Inc =>
            Registers (Instruction.Register) := @ + 1;
            Instruction_Pointer := @ + 1;

         when Jmp =>
            Instruction_Pointer := @ + Instruction.Offset;

         when Jie =>
            if Registers (Instruction.Test_Register) mod 2 = 0 then
               Instruction_Pointer := @ + Instruction.Offset;
            else
               Instruction_Pointer := @ + 1;
            end if;

         when Jio =>
            if Registers (Instruction.Test_Register) = 1 then
               Instruction_Pointer := @ + Instruction.Offset;
            else
               Instruction_Pointer := @ + 1;
            end if;

         when Stop =>
            raise End_Of_Program;

      end case;
   end Interpret;

   function Solution (Initial_A : Natural := 0) return Natural is
      Memory              : constant Memory_Array := Memory_Start;
      Instruction_Pointer : Memory_Range := Memory_Range'First;
      Registers           : Register_Array := [A => Initial_A, B => 0];
   begin
      while Memory (Instruction_Pointer).Kind /= Stop loop
         Interpret (Instruction_Pointer, Memory, Registers);
      end loop;
      return Registers (B);
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("After running the program, the value in register b is"
      & Solution'Image);
   IO.Put_Line
     ("...but if we initialize register a to 1, it will be"
      & Solution (1)'Image);
end Day23;
