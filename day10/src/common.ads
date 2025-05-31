--  Advent of Code 2015
--  John Perry
--
--  declarations for common types and constants
--  there is no implementation

pragma Ada_2022;

with Ada.Containers.Vectors;

package Common is

   package Positive_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);

   subtype Positive_Vector is Positive_Vecs.Vector;

   Puzzle_Input : constant Positive_Vector := [3, 1, 1, 3, 3, 2, 2, 1, 1, 3];

end Common;
