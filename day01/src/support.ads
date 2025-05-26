--  2015 Advent of Code in Ada, Day 01 (in SPARK!)
--  John Perry
--
--  SPARK-checked subprograms: Part_1 and Part_2: declaration
--
--  also declares the types they need

with SPARK.Containers.Formal.Unbounded_Vectors;

package Support is

   type Direction is (Up, Dn);

   package Direction_Vectors is new
     --    Ada.Containers.Vectors
     SPARK.Containers.Formal.Unbounded_Vectors
       (Index_Type   => Positive,
        Element_Type => Direction);

   subtype Direction_Vector is Direction_Vectors.Vector;

   function Part_1 (Directions : Direction_Vector) return Integer
   with SPARK_Mode => On;
   --  returns the floor the directions would take us to

   function Part_2 (Directions : Direction_Vector) return Positive
   with SPARK_Mode => On;
   --  returns the first step where the directions take us to the basement

end Support;
