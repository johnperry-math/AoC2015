--  Advent of Code 2015
--  John Perry
--
--  help Santa solve a traveline salesman problem

pragma Ada_2022;

with Ada.Text_IO;

procedure Day09 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   type Destination is
     (Alpha_Centauri,
      Snowdin,
      Tambi,
      Faerun,
      Norrath,
      Straylight,
      Tristram,
      Arbre);

   type Map_Array is array (Destination, Destination) of Positive;
   Map : Map_Array;

   --  SECTION
   --  I/O

   Invalid_Destination : exception;

   function Identify_Destination (Value : String) return Destination
   is (if Value = "AlphaCentauri"
       then Alpha_Centauri
       elsif Value = "Snowdin"
       then Snowdin
       elsif Value = "Tambi"
       then Tambi
       elsif Value = "Faerun"
       then Faerun
       elsif Value = "Norrath"
       then Norrath
       elsif Value = "Straylight"
       then Straylight
       elsif Value = "Tristram"
       then Tristram
       elsif Value = "Arbre"
       then Arbre
       else raise Invalid_Destination);

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop
         declare
            Line                  : constant String := IO.Get_Line (Input);
            First_End, Second_End : Positive;
         begin
            First_End := Line'First;
            while Line (First_End + 1) /= ' ' loop
               First_End := @ + 1;
            end loop;
            Second_End := First_End + 5;
            while Line (Second_End + 1) /= ' ' loop
               Second_End := @ + 1;
            end loop;
            declare
               Start    : constant Destination :=
                 Identify_Destination (Line (Line'First .. First_End));
               Stop     : constant Destination :=
                 Identify_Destination (Line (First_End + 5 .. Second_End));
               Distance : constant Positive :=
                 Positive'Value (Line (Second_End + 4 .. Line'Last));
            begin
               Map (Start, Stop) := Distance;
               Map (Stop, Start) := Distance;
            end;
         end;
      end loop;

      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2
   --
   --  this is a brute-force solution. i'm not proud of it,
   --  but there aren't enough destinations to warrant
   --  something more sophisticated.

   subtype Route_Indices is
     Positive
       range 1
             .. (Destination'Pos (Destination'Last)
                 - Destination'Pos (Destination'First)
                 + 1);
   --  i wish ada had a nicer way to count the items in an enumeration

   type Route_Array is array (Route_Indices) of Destination;
   --  we store potential routes in this array

   generic
      Shortest : Boolean := True;
   function Complete
     (Route : in out Route_Array; Ith : Route_Indices; Map : Map_Array)
      return Positive;

   function Complete
     (Route : in out Route_Array; Ith : Route_Indices; Map : Map_Array)
      return Positive
   is
      --  finds `Route`'s optimal completion,
      --  where `Ith` is its last assigned position:
      --  we minimize when `Shortest` is true, and maximize otherwise

      Result         : Natural;
      Route_Distance : Positive;

   begin

      if Ith = Route'Last then
         --  we're at the end! return the result
         Result := 0;
         for Jth in Route'First .. Route'Last - 1 loop
            Result := @ + Map (Route (Jth), Route (Jth + 1));
         end loop;

      else
         --  find the smallest route obtained from existing route,
         --  without adding destinations that already appear along the route
         Result := (if Shortest then Positive'Last else 1);
         for Next
           in Destination
           when (for all Jth in Route'First .. Ith => Route (Jth) /= Next)
         loop
            --  i could optimize a little by skipping Next
            --  when the total distance would exceed the current result,
            --  but that would require more effort than this seems to warrant
            Route (Ith + 1) := Next;
            Route_Distance := Complete (Route, Ith + 1, Map);
            if (Shortest and then Route_Distance < Result)
              or else ((not Shortest) and then Route_Distance > Result)
            then
               Result := Route_Distance;
            end if;
         end loop;
      end if;
      return Result;
   end Complete;

   function Part_1 return Positive is
      --  minimize the route
      Result : Positive := Positive'Last;
      Route  : Route_Array;
      function Minimize is new Complete (Shortest => True);
   begin
      for Start in Destination loop
         Route (1) := Start;
         Result := Positive'Min (Result, Minimize (Route, 1, Map));
      end loop;
      return Result;
   end Part_1;

   function Part_2 return Positive is
      --  maximize the route
      Result : Positive := Positive'First;
      Route  : Route_Array;
      function Maximize is new Complete (Shortest => False);
   begin
      for Start in Destination loop
         Route (1) := Start;
         Result := Positive'Max (Result, Maximize (Route, 1, Map));
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("The shortest route has distance" & Part_1'Image);
   IO.Put_Line ("The longest route has distance" & Part_2'Image);
end Day09;
