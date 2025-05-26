--  2015 Advent of Code in Ada, Day 02
--  John Perry
--
--  The Elves are packing.
--  (No, they're not leaving. They're wrapping boxes.)

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Day02 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   --  SECTION: global types, variables, and utility functions

   type Package_Dimensions is record
      Length, Width, Height : Positive;
   end record;

   package Package_Dimension_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Package_Dimensions);

   All_Packages : Package_Dimension_Vectors.Vector;

   function Area
     (Box : Package_Dimensions)
      return Positive
             --  returns the surface area, plus a little extra,
             --  which is the smallest side area
   is (2
       * Box.Length
       * Box.Width
       + 2 * Box.Length * Box.Height
       + 2 * Box.Width * Box.Height
       + Integer'Min
           (Box.Length * Box.Width,
            Integer'Min (Box.Length * Box.Height, Box.Width * Box.Height)));

   function Perimeter
     (Box : Package_Dimensions)
      return Positive
             --  returns the minimum perimeter of a side
   is (2
       * Integer'Min
           (Box.Length + Box.Width,
            Integer'Min (Box.Length + Box.Height, Box.Width + Box.Height)));

   function Volume
     (Box : Package_Dimensions)
      return Positive
             --  at least this one does what you'd expect :-)
   is (Box.Length * Box.Width * Box.Height);

   --  SECTION: I/O

   procedure Read_Input is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, "input.txt");
      while not IO.End_Of_File (File) loop
         declare
            Line     : constant String := IO.Get_Line (File);
            First_X  : constant Positive := Strings.Index (Line, "x");
            Second_X : constant Positive :=
              Strings.Index (Line, "x", First_X + 1);
            Length   : constant Positive :=
              Positive'Value (Line (Line'First .. First_X - 1));
            Width    : constant Positive :=
              Positive'Value (Line (First_X + 1 .. Second_X - 1));
            Height   : constant Positive :=
              Positive'Value (Line (Second_X + 1 .. Line'Last));
         begin
            All_Packages.Append (Package_Dimensions'(Length, Width, Height));
         end;
      end loop;
      IO.Close (File);
   end Read_Input;

   --  SECTION; parts 1 and 2

   function Part_1 return Positive is
      --  total area; or really, total amount of wrapping paper needed
      Result : Natural := 0;
   begin
      for Box of All_Packages loop
         Result := @ + Area (Box);
      end loop;
      return Result;
   end Part_1;

   function Part_2 return Positive is
      --  total amount of ribbon needed, determined by "perimeter" and volume
      Result : Natural := 0;
   begin
      for Box of All_Packages loop
         Result := @ + Perimeter (Box) + Volume (Box);
      end loop;
      return Result;
   end Part_2;

   --  SECTION: main

begin
   Read_Input;
   IO.Put_Line
     ("The elves need" & Part_1'Image & " square feet of wrapping paper");
   IO.Put_Line ("The elves need" & Part_2'Image & " feet of ribbon");
end Day02;
