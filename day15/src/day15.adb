--  Advent of Code 2015
--  John Perry
--
--  make the best Christmas cookie EVAH

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day15 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   package Integer_IO is new IO.Integer_IO (Num => Integer);

   --  SECTION
   --  global types and variables

   type Ingredient_Enum is (Sugar, Sprinkles, Candy, Chocolate);
   type Property_Enum is (Capacity, Durability, Flavor, Texture, Calories);
   subtype Non_Caloric is Property_Enum range Capacity .. Texture;

   Ingredient_Properties : array (Ingredient_Enum, Property_Enum) of Integer;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for Ingredient in Ingredient_Enum loop
         declare
            Line        : constant String := IO.Get_Line (Input);
            Space_Index : Positive := Line'First;
         begin
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Integer_IO.Get
              (Line (Space_Index + 1 .. Line'Last),
               Ingredient_Properties (Ingredient, Capacity),
               Space_Index);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Integer_IO.Get
              (Line (Space_Index + 1 .. Line'Last),
               Ingredient_Properties (Ingredient, Durability),
               Space_Index);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Integer_IO.Get
              (Line (Space_Index + 1 .. Line'Last),
               Ingredient_Properties (Ingredient, Flavor),
               Space_Index);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Integer_IO.Get
              (Line (Space_Index + 1 .. Line'Last),
               Ingredient_Properties (Ingredient, Texture),
               Space_Index);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Space_Index := Strings.Index (Line, " ", Space_Index + 1);
            Integer_IO.Get
              (Line (Space_Index + 1 .. Line'Last),
               Ingredient_Properties (Ingredient, Calories),
               Space_Index);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  types

   type Recipe_Array is array (Ingredient_Enum) of Natural;
   type Fixed_Array is array (Ingredient_Enum) of Boolean;
   type Part_Enum is (First, Second);

   Limit : constant Positive := 50;
   --  seems unlikely we will need more than 50 teaspoons of anything
   --  this may be false for some inputs!

   --  SUBSECTION
   --  evaluating properties and recipes

   function Property_Score
     (Property : Property_Enum; Recipe_Amt : Recipe_Array) return Integer
   is
      --  returns the score of the recipe for the given property
      Result : Integer := 0;
   begin
      for Ingredient in Ingredient_Enum loop
         Result :=
           @
           + Recipe_Amt (Ingredient)
             * Ingredient_Properties (Ingredient, Property);
      end loop;
      return Integer'Max (0, Result);
   end Property_Score;

   function Evaluate (Recipe : Recipe_Array) return Integer is
      --  returns the total score of the recipe across all properties
      Result : Integer := 1;
   begin
      for Property in Non_Caloric loop
         Result := @ * Property_Score (Property, Recipe);
      end loop;
      return Result;
   end Evaluate;

   --  SUBSECTION
   --  double recursion

   function Best_Score
     (Recipe    : Recipe_Array;
      Available : Natural;
      Fixed     : Fixed_Array;
      Part      : Part_Enum) return Natural;

   function Complete
     (Recipe : Recipe_Array; Fixed : Fixed_Array; Part : Part_Enum)
      return Natural
   is
      --  extends Recipe to non-Fixed ingredients in brute force fashin,
      --  returning the best recipe score possible given what's fixed
      --
      --  when Part is Second, restricts the recipes
      --  to those whose calorie count is 500
   begin
      if (for all Ingredient in Ingredient_Enum => Fixed (Ingredient)) then
         return
           (if Part = First or else Property_Score (Calories, Recipe) = 500
            then Evaluate (Recipe)
            else 0);
      else
         declare
            Result    : Natural := 0;
            Available : Natural := 100;
         begin
            --  determine how many teaspoons are available
            for Ingredient in Ingredient_Enum loop
               if Fixed (Ingredient) then
                  Available := @ - Recipe (Ingredient);
               end if;
            end loop;
            --  divide the available amount among non-fixed ingredients
            --  and determine the highest-scoring result
            return Best_Score (Recipe, Available, Fixed, Part);
         end;
      end if;
   end Complete;

   function Best_Score
     (Recipe    : Recipe_Array;
      Available : Natural;
      Fixed     : Fixed_Array;
      Part      : Part_Enum) return Natural
   is
      --  best score given a partially-completed Recipe
      --  with certain ingredients already Fixed
      --  and a certain number of teaspoons Available
      Result          : Natural := 0;
      More_Quantities : Recipe_Array := Recipe;
      More_Fixed      : Fixed_Array := Fixed;
   begin
      for Ingredient in Ingredient_Enum when not Fixed (Ingredient) loop
         --  fix the ingredient, loop through its possible values
         --  (up to a reasonable limit) and complete
         More_Fixed (Ingredient) := True;
         for Amount in 5 .. Positive'Min (Limit, Available) loop
            More_Quantities (Ingredient) := Amount;
            Result :=
              Natural'Max
                (Result, Complete (More_Quantities, More_Fixed, Part));
         end loop;
         More_Fixed (Ingredient) := False;
         More_Quantities (Ingredient) := 0;
      end loop;
      return Result;
   end Best_Score;

   --  SUBSECTION
   --  the big-picture view of the solution

   function Solution (Part : Part_Enum) return Natural is
      --  solves the puzzle with the given Part
      Result : Natural := 0;
   begin
      for Ingredient in Ingredient_Enum loop
         declare
            Fixed    : Fixed_Array := [others => False];
            Quantity : Recipe_Array := [others => 0];
         begin
            Fixed (Ingredient) := True;
            for Amount in 5 .. Limit loop
               Quantity (Ingredient) := Amount;
               Result :=
                 Natural'Max (Result, Complete (Quantity, Fixed, Part));
            end loop;
         end;
      end loop;
      return Result;
   end Solution;

   --  SECTION
   --  main

begin
   Read_Input;
   IO.Put_Line
     ("The highest-scoring cookie has score" & Solution (First)'Image);
   IO.Put_Line
     ("The highest-scoring cookie has score" & Solution (Second)'Image);
end Day15;
