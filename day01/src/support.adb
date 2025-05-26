--  2015 Advent of Code in Ada, Day 01 (in SPARK!)
--  John Perry
--
--  SPARK-checked subprograms: Part_1 and Part_2: implementation

with SPARK.Containers.Formal.Unbounded_Vectors;

with Log;

package body Support is

   use all type Direction_Vector;

   function Part_1 (Directions : Direction_Vector) return Integer
   with SPARK_Mode => On
   is
      Result : Integer := 0;
   begin
      for Dir of Directions loop
         case Dir is
            when Up =>
               if Result < Integer'Last then
                  Result := @ + 1;
               else
                  Log.Info ("Result invalid: overflow");
               end if;

            when Dn =>
               if Result > Integer'First then
                  Result := @ - 1;
               else
                  Log.Info ("Result invalid: underflow");
               end if;
         end case;
      end loop;
      return Result;
   end Part_1;

   function Part_2 (Directions : Direction_Vector) return Positive is
      Result        : Natural := 0;
      Current_Floor : Integer := 0;
   begin
      while Current_Floor >= 0 loop
         Result := @ + 1;
         case Element (Directions, Result) is
            when Up =>
               Current_Floor := @ + 1;

            when Dn =>
               Current_Floor := @ - 1;
         end case;
      end loop;
      return Result;
   end Part_2;

end Support;
