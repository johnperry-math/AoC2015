--  2015 Advent of Code in Ada, Day 01 (in SPARK!)
--  John Perry
--
--  Wrapper for I/O from SPARK functions: implementation

with Ada.Text_IO;

package body Log is

   procedure Info (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Text);
   end Info;

end Log;
