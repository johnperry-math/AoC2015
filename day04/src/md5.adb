--  Advent of Code 2015
--  John Perry
--
--  Implementation of MD5 support
--
--  This is essentially a translation of
--  https://stackoverflow.com/a/997309/4526030
--  which looks in turn to be pretty close to the reference implementation
--  in the RFC.

pragma Ada_2022;

with Ada.Text_IO;

package body MD5 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  common types not already defined

   package Byte_IO is new Ada.Text_IO.Modular_IO (Num => MD5.Byte);

   use all type Byte;

   subtype Word is Interfaces.Unsigned_32;
   --  words are used for chunking
   use all type Word;

   type Word_Array is array (Natural range <>) of Word;

   --  SECTION
   --  conversion utilities

   function Text_To_Byte_Array (Text : String) return Byte_Array is
      Result : constant Byte_Array (0 .. Text'Length - 1) :=
        [for Ith in Text'Range => Byte (Character'Pos (Text (Ith)))];
   begin
      return Result;
   end Text_To_Byte_Array;

   function Hash_Array_To_Text (Hash : Hash_Array) return Hash_String is
      Result : Hash_String;
      Temp   : String (1 .. 6);
   begin
      for Ith in Hash'Range loop
         --  first use Ada to create an overly formatted string
         Byte_IO.Put (Temp, Hash (Ith), 16);
         -- watch out for single-character values!
         Result (Ith * 2 + 1 .. Ith * 2 + 2) :=
           (if Hash (Ith) < 16 then "0" & Temp (5) else Temp (4 .. 5));
      end loop;
      return Result;
   end Hash_Array_To_Text;

   function Byte_To_Word_Array (Text : Byte_Array) return Word_Array is
      Length : constant Natural := Text'Length;

      --  these lines determine the amount of padding needed
      Number_Of_Words_1 : constant Positive := Length + 8;
      Number_Of_Words_2 : constant Natural :=
        (Number_Of_Words_1 - (Number_Of_Words_1 mod 64)) / 64;

      --  now we can figure how many words we need
      Number_Of_Words : constant Natural := (Number_Of_Words_2 + 1) * 16;
      Result          : Word_Array (0 .. Number_Of_Words - 1) :=
        [others => 16#00#];

      Word_Count, Byte_Count, Position : Natural := 0;
   begin
      --  notice that this technically reverses the values,
      --  putting them into little-endian format
      while Byte_Count < Length loop
         Word_Count := (Byte_Count - (Byte_Count mod 4)) / 4;
         Position := (Byte_Count mod 4) * 8;
         Result (Word_Count) :=
           (Result (Word_Count)
            or Interfaces.Shift_Left (Word (Text (Byte_Count)), Position));
         Byte_Count := @ + 1;
      end loop;

      --  tag with a 1, then add the length at the end
      Word_Count := (Byte_Count - (Byte_Count mod 4)) / 4;
      Position := (Byte_Count mod 4) * 8;
      Result (Word_Count) :=
        Result (Word_Count) or Interfaces.Shift_Left (16#80#, Position);
      Result (Number_Of_Words - 2) := Interfaces.Shift_Left (Word (Length), 3);
      Result (Number_Of_Words - 1) :=
        Interfaces.Shift_Right (Word (Length), 29);
      return Result;
   end Byte_To_Word_Array;

   function Word_To_Hash_Array (Values : Word_Array) return Hash_Array is
      Result : Hash_Array := [others => 0];
      Item   : Byte;
      Value  : Word;
   begin
      for Ith in 0 .. 3 loop
         Value := Values (Ith);
         for Count in 0 .. 3 loop
            Item := Byte (Interfaces.Shift_Right (Value, Count * 8) and 255);
            Result (Ith * 4 + Count) := Item;
         end loop;
      end loop;
      return Result;
   end Word_To_Hash_Array;

   --  SECTION
   --  utilities for computation
   --  don't ask me what they're supposed to do; perhaps the RFC explains it

   function Common_Apply (Q, A, B, X, Ac : Word; S : Natural) return Word
   is (Interfaces.Rotate_Left ((A + Q) + (X + Ac), S) + B);

   function FF (A, B, C, D, X, Ac : Word; S : Natural) return Word
   is (Common_Apply (((B and C) or ((not B) and D)), A, B, X, Ac, S));

   function GG (A, B, C, D, X, Ac : Word; S : Natural) return Word
   is (Common_Apply (((B and D) or (C and (not D))), A, B, X, Ac, S));

   function HH (A, B, C, D, X, Ac : Word; S : Natural) return Word
   is (Common_Apply (B xor C xor D, A, B, X, Ac, S));

   function II (A, B, C, D, X, Ac : Word; S : Natural) return Word
   is (Common_Apply (C xor (B or (not D)), A, B, X, Ac, S));

   --  SECTIONO
   --  The point!

   function Hash (Text : Byte_Array) return Hash_Array is

      Result : Hash_Array;

      A0 : Word := 16#67452301#;
      B0 : Word := 16#efcdab89#;
      C0 : Word := 16#98badcfe#;
      D0 : Word := 16#10325476#;

      Position : Integer := 0;

      Workspace : constant Word_Array := Byte_To_Word_Array (Text);

   begin
      while Position < Workspace'Length loop
         declare
            A : Word := A0;
            B : Word := B0;
            C : Word := C0;
            D : Word := D0;
         begin
            A := FF (A, B, C, D, Workspace (Position + 0), 16#D76AA478#, 7);
            D := FF (D, A, B, C, Workspace (Position + 1), 16#E8C7B756#, 12);
            C := FF (C, D, A, B, Workspace (Position + 2), 16#242070DB#, 17);
            B := FF (B, C, D, A, Workspace (Position + 3), 16#C1BDCEEE#, 22);
            A := FF (A, B, C, D, Workspace (Position + 4), 16#F57C0FAF#, 7);
            D := FF (D, A, B, C, Workspace (Position + 5), 16#4787C62A#, 12);
            C := FF (C, D, A, B, Workspace (Position + 6), 16#A8304613#, 17);
            B := FF (B, C, D, A, Workspace (Position + 7), 16#FD469501#, 22);
            A := FF (A, B, C, D, Workspace (Position + 8), 16#698098D8#, 7);
            D := FF (D, A, B, C, Workspace (Position + 9), 16#8B44F7AF#, 12);
            C := FF (C, D, A, B, Workspace (Position + 10), 16#FFFF5BB1#, 17);
            B := FF (B, C, D, A, Workspace (Position + 11), 16#895CD7BE#, 22);
            A := FF (A, B, C, D, Workspace (Position + 12), 16#6B901122#, 7);
            D := FF (D, A, B, C, Workspace (Position + 13), 16#FD987193#, 12);
            C := FF (C, D, A, B, Workspace (Position + 14), 16#A679438E#, 17);
            B := FF (B, C, D, A, Workspace (Position + 15), 16#49B40821#, 22);
            A := GG (A, B, C, D, Workspace (Position + 1), 16#F61E2562#, 5);
            D := GG (D, A, B, C, Workspace (Position + 6), 16#C040B340#, 9);
            C := GG (C, D, A, B, Workspace (Position + 11), 16#265E5A51#, 14);
            B := GG (B, C, D, A, Workspace (Position + 0), 16#E9B6C7AA#, 20);
            A := GG (A, B, C, D, Workspace (Position + 5), 16#D62F105D#, 5);
            D := GG (D, A, B, C, Workspace (Position + 10), 16#2441453#, 9);
            C := GG (C, D, A, B, Workspace (Position + 15), 16#D8A1E681#, 14);
            B := GG (B, C, D, A, Workspace (Position + 4), 16#E7D3FBC8#, 20);
            A := GG (A, B, C, D, Workspace (Position + 9), 16#21E1CDE6#, 5);
            D := GG (D, A, B, C, Workspace (Position + 14), 16#C33707D6#, 9);
            C := GG (C, D, A, B, Workspace (Position + 3), 16#F4D50D87#, 14);
            B := GG (B, C, D, A, Workspace (Position + 8), 16#455A14ED#, 20);
            A := GG (A, B, C, D, Workspace (Position + 13), 16#A9E3E905#, 5);
            D := GG (D, A, B, C, Workspace (Position + 2), 16#FCEFA3F8#, 9);
            C := GG (C, D, A, B, Workspace (Position + 7), 16#676F02D9#, 14);
            B := GG (B, C, D, A, Workspace (Position + 12), 16#8D2A4C8A#, 20);
            A := HH (A, B, C, D, Workspace (Position + 5), 16#FFFA3942#, 4);
            D := HH (D, A, B, C, Workspace (Position + 8), 16#8771F681#, 11);
            C := HH (C, D, A, B, Workspace (Position + 11), 16#6D9D6122#, 16);
            B := HH (B, C, D, A, Workspace (Position + 14), 16#FDE5380C#, 23);
            A := HH (A, B, C, D, Workspace (Position + 1), 16#A4BEEA44#, 4);
            D := HH (D, A, B, C, Workspace (Position + 4), 16#4BDECFA9#, 11);
            C := HH (C, D, A, B, Workspace (Position + 7), 16#F6BB4B60#, 16);
            B := HH (B, C, D, A, Workspace (Position + 10), 16#BEBFBC70#, 23);
            A := HH (A, B, C, D, Workspace (Position + 13), 16#289B7EC6#, 4);
            D := HH (D, A, B, C, Workspace (Position + 0), 16#EAA127FA#, 11);
            C := HH (C, D, A, B, Workspace (Position + 3), 16#D4EF3085#, 16);
            B := HH (B, C, D, A, Workspace (Position + 6), 16#4881D05#, 23);
            A := HH (A, B, C, D, Workspace (Position + 9), 16#D9D4D039#, 4);
            D := HH (D, A, B, C, Workspace (Position + 12), 16#E6DB99E5#, 11);
            C := HH (C, D, A, B, Workspace (Position + 15), 16#1FA27CF8#, 16);
            B := HH (B, C, D, A, Workspace (Position + 2), 16#C4AC5665#, 23);
            A := II (A, B, C, D, Workspace (Position + 0), 16#F4292244#, 6);
            D := II (D, A, B, C, Workspace (Position + 7), 16#432AFF97#, 10);
            C := II (C, D, A, B, Workspace (Position + 14), 16#AB9423A7#, 15);
            B := II (B, C, D, A, Workspace (Position + 5), 16#FC93A039#, 21);
            A := II (A, B, C, D, Workspace (Position + 12), 16#655B59C3#, 6);
            D := II (D, A, B, C, Workspace (Position + 3), 16#8F0CCC92#, 10);
            C := II (C, D, A, B, Workspace (Position + 10), 16#FFEFF47D#, 15);
            B := II (B, C, D, A, Workspace (Position + 1), 16#85845DD1#, 21);
            A := II (A, B, C, D, Workspace (Position + 8), 16#6FA87E4F#, 6);
            D := II (D, A, B, C, Workspace (Position + 15), 16#FE2CE6E0#, 10);
            C := II (C, D, A, B, Workspace (Position + 6), 16#A3014314#, 15);
            B := II (B, C, D, A, Workspace (Position + 13), 16#4E0811A1#, 21);
            A := II (A, B, C, D, Workspace (Position + 4), 16#F7537E82#, 6);
            D := II (D, A, B, C, Workspace (Position + 11), 16#BD3AF235#, 10);
            C := II (C, D, A, B, Workspace (Position + 2), 16#2AD7D2BB#, 15);
            B := II (B, C, D, A, Workspace (Position + 9), 16#EB86D391#, 21);
            A0 := A0 + A;
            B0 := B0 + B;
            C0 := C0 + C;
            D0 := D0 + D;
         end;
         Position := @ + 16;
      end loop;

      Result := Word_To_Hash_Array ([A0, B0, C0, D0]);

      return Result;
   end Hash;

end MD5;
