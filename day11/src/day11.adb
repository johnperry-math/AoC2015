--  Advent of Code 2015
--  John Perry
--
--  help Santa find his next, very-insecure password

pragma Ada_2022;

with Ada.Text_IO;

procedure Day11 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   subtype Password_Range is Positive range 1 .. 8;

   subtype Password_Array is String (Password_Range);

   --  SECTION
   --  password requirements

   Exhausted_Passwords : exception;

   function Step (Password : Password_Array) return Password_Array is
      --  returns the next password that comes from incrementing a letter
      --  and carrying any overflow to the next letter.
      --  raises the Exhausted_Passwords exception when handed "zzzzzzzz"
      Result   : Password_Array := Password;
      Position : Password_Range := Password_Range'Last;
   begin
      while Position > Password_Range'First and then Result (Position) = 'z'
      loop
         Result (Position) := 'a';
         Position := @ - 1;
      end loop;
      if Position = Password_Range'First and then Result (Position) = 'z' then
         raise Exhausted_Passwords;
      end if;
      Result (Position) := Character'Succ (Result (Position));
      return Result;
   end Step;

   function Has_Straight (Password : Password_Array) return Boolean
   is (for some Ith in Password_Range'First .. Password_Range'Last - 2
       => Password (Ith + 1) = Character'Succ (Password (Ith))
          and then Password (Ith + 2) = Character'Succ (Password (Ith + 1)));
   --  returns True iff there is a sequence of three successive characters

   function Not_Confusing (Password : Password_Array) return Boolean
   is (for all Letter of Password
       => Letter /= 'i' and then Letter /= 'o' and then Letter /= 'l');
   --  returns True iff Password does not contains i, o, or l

   function Has_Two_Pair (Password : Password_Array) return Boolean
   is (for some Ith in Password_Range'First .. Password_Range'Last - 3
       => ((Password (Ith) = Password (Ith + 1))
           and then (for some Jth in Ith + 2 .. Password_Range'Last - 1
                     => Password (Jth) = Password (Jth + 1))));
   --  returns True iff Password has two non-overlapping pairs

   function Next (Password : Password_Array) return Password_Array is
      --  returns the next valid password after the one given
      Result : Password_Array := Password;
   begin
      loop
         Result := Step (Result);
         exit when
           Has_Straight (Result)
           and then Not_Confusing (Result)
           and then Has_Two_Pair (Result);
      end loop;
      return Result;
   end Next;

   Initial_Password : constant Password_Array := "hxbxwxba";

begin
   IO.Put_Line ("The next password is " & Next (Initial_Password));
   IO.Put_Line ("The one after that is " & Next (Next (Initial_Password)));

end Day11;
