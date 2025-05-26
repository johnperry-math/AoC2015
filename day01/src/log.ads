--  2015 Advent of Code in Ada, Day 01 (in SPARK!)
--  John Perry
--
--  Wrapper for I/O from SPARK functions: declaration

package Log is

   procedure Info (Text : String)
   with Always_Terminates => True;

end Log;
