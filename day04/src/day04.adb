--  Advent of Code 2015
--  John Perry
--
--  Use MD5 (Wow, on day 4?!?) to help mine AdventCoins

pragma Ada_2022;

with Ada.Text_IO;

with MD5;

procedure Day04 is
   package IO renames Ada.Text_IO;

   --  QBD was used for testing
   QBD_String : constant String :=
     "The quick brown fox jumps over the lazy dog";
   --  QBD_String : constant String := "The quick brown fox jumps over the lazy dog.";
   --  QBD_String : constant String := "";
   QBD_Array  : constant MD5.Byte_Array := MD5.Text_To_Byte_Array (QBD_String);
   QBD_Hash   : constant MD5.Hash_Array := MD5.Hash (QBD_Array);

   Doing_Example : constant Boolean := False;
   Input_String  : constant String :=
     (if Doing_Example then "abcdef" else "yzbqklnj");

   Whoops : exception;

   function Part_1 return Positive is
      --  maybe there's a smarter way to do this than brute force,
      --  but i spent enough time banging my head against MD5
   begin
      for Ith in 1 .. 1_000_000 loop
         declare
            Ith_Image : constant String := Ith'Image;
            Text      : constant String :=
              Input_String & Ith_Image (2 .. Ith_Image'Last);
            Hash      : constant String :=
              MD5.Hash_Array_To_Text
                (MD5.Hash (MD5.Text_To_Byte_Array (Text)));
         begin
            if Ith mod 100_000 = 0 then
               IO.Put_Line (Ith'Image & " " & Text);
            end if;

            if Hash (1 .. 5) = "00000" then
               return Ith;
            end if;
         end;
      end loop;
      raise Whoops;
   end Part_1;

   function Part_2 return Positive is
   begin
      for Ith in 1 .. 1_000_000_000 loop
         declare
            Ith_Image : constant String := Ith'Image;
            Text      : constant String :=
              Input_String & Ith_Image (2 .. Ith_Image'Last);
            Hash      : constant String :=
              MD5.Hash_Array_To_Text
                (MD5.Hash (MD5.Text_To_Byte_Array (Text)));
         begin
            if Ith mod 100_000 = 0 then
               IO.Put_Line (Ith'Image & " " & Text);
            end if;

            if Hash (1 .. 6) = "000000" then
               return Ith;
            end if;
         end;
      end loop;
      raise Whoops;
   end Part_2;

begin
   if Doing_Example then
      IO.Put_Line (MD5.Hash_Array_To_Text (QBD_Hash));
   end if;
   IO.Put_Line
     ("The lowest positive number producing a hash for "
      & Input_String
      & " is"
      & Part_1'Image);
   IO.Put_Line ("Er no, maybe it's " & Part_2'Image);
end Day04;
