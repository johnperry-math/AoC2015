--  2015 Advent of Code in Ada, Day 03
--  John Perry
--
--  Santa is delivering presents,
--  and we want to know how many houses he visits.

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

procedure Day03 is

   package IO renames Ada.Text_IO;

   use all type Ada.Containers.Hash_Type;

   --  SECTION: global types and variables

   --  SUBSECTION: directions

   type Direction is (Up, Dn, Lt, Rt);

   package Direction_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Direction);

   Instructions : Direction_Vectors.Vector;

   --  SUBSECTION: locations

   type Location_Record is record
      Row, Column : Integer;
   end record;

   function Hash (Item : Location_Record) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (abs (Item.Row))
       * Ada.Containers.Hash_Type (abs (Item.Column)));

   package Location_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Location_Record,
        Hash                => Hash,
        Equivalent_Elements => "=");

   --  SECTION: I/O

   Invalid_Direction : exception;

   procedure Read_Input is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, "input.txt");
      declare
         Line     : constant String := IO.Get_Line (File);
         Position : Positive := Line'First;
      begin
         while Position <= Line'Last loop
            case Line (Position) is
               when '^' =>
                  Instructions.Append (Up);

               when 'v' =>
                  Instructions.Append (Dn);

               when '<' =>
                  Instructions.Append (Lt);

               when '>' =>
                  Instructions.Append (Rt);

               when others =>
                  raise Invalid_Direction;
            end case;
            Position := @ + 1;
         end loop;
      end;
      IO.Close (File);
   end Read_Input;

   --  SECTION: Part 1
   --  count the number of locations visited by one agent

   function Part_1 return Positive is
      Current_Location : Location_Record := (0, 0);
      Santa_Locations  : Location_Sets.Set;
   begin
      Santa_Locations.Include (Current_Location);
      for Instruction of Instructions loop
         case Instruction is
            when Up =>
               Current_Location.Row := @ - 1;

            when Dn =>
               Current_Location.Row := @ + 1;

            when Lt =>
               Current_Location.Column := @ - 1;

            when Rt =>
               Current_Location.Column := @ + 1;
         end case;
         Santa_Locations.Include (Current_Location);
      end loop;
      return Positive (Santa_Locations.Length);
   end Part_1;

   --  SECTION
   --  Part 2: count the number of locations visited by two agents
   --  who alternate the instructions

   function Part_2 return Positive is
      Santas_Location, Robots_Locations : aliased Location_Record := (0, 0);
      All_Locations                     : Location_Sets.Set;
      Ith                               : Positive := Instructions.First_Index;
      Location                          : access Location_Record;
   begin
      All_Locations.Include (Santas_Location);
      while Ith <= Instructions.Last_Index loop
         Location :=
           (if Ith mod 2 = 0
            then Santas_Location'Access
            else Robots_Locations'Access);
         case Instructions (Ith) is
            when Up =>
               Location.Row := @ - 1;

            when Dn =>
               Location.Row := @ + 1;

            when Lt =>
               Location.Column := @ - 1;

            when Rt =>
               Location.Column := @ + 1;

            when others =>
               raise Invalid_Direction;
         end case;
         All_Locations.Include (Location.all);
         Ith := @ + 1;
      end loop;
      return Positive (All_Locations.Length);
   end Part_2;

   --  SECTION: main

begin
   Read_Input;
   IO.Put_Line ("Santa visits" & Part_1'Image & " houses");
   IO.Put_Line ("Santa and a robot would visit" & Part_2'Image & " houses");
end Day03;
