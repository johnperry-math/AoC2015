--  Advent of Code 2015
--  John Perry
--
--  Declarations for MD5 support (Wow, on day 4?!?)

with Interfaces;

package MD5 is

   subtype Byte is Interfaces.Unsigned_8;

   type Byte_Array is array (Natural range <>) of Byte;

   type Hash_Array is array (0 .. 15) of Byte;
   --  MD5 produces a 16-byte signature

   subtype Hash_String is String (1 .. 32);
   --  the 16-byte Hash_Array corresponds to a 32-character String

   function Text_To_Byte_Array (Text : String) return Byte_Array;
   --  converts Text to a Byte_Array suitable for Hash-ing

   function Hash (Text : Byte_Array) return Hash_Array;
   --  returns the MD5 hash of Text

   function Hash_Array_To_Text (Hash : Hash_Array) return Hash_String;
   --  converts Hash to a String suitable for I/O

end MD5;
