--  Advent of Code 2015
--  John Perry
--
--  declarations for atomic expansion of the look-and-say Count

pragma Ada_2022;

with Ada.Containers.Vectors;

with Common;

package Atomic is

   subtype Positive_Vector is Common.Positive_Vector;

   type Element_Enum is
   --  list of elements taken from
   --  https://en.wikipedia.org/wiki/Look-and-say_sequence#Cosmological_decay
     (H,
      He,
      Li,
      Be,
      B,
      C,
      N,
      O,
      F,
      Ne,
      Na,
      Mg,
      Al,
      Si,
      P,
      S,
      Cl,
      Ar,
      K,
      Ca,
      Sc,
      Ti,
      V,
      Cr,
      Mn,
      Fe,
      Co,
      Ni,
      Cu,
      Zn,
      Ga,
      Ge,
      As,
      Se,
      Br,
      Kr,
      Rb,
      Sr,
      Y,
      Zr,
      Nb,
      Mo,
      Tc,
      Ru,
      Rh,
      Pd,
      Ag,
      Cd,
      In_Atom,
      Sn,
      Sb,
      Te,
      I,
      Xe,
      Cs,
      Ba,
      La,
      Ce,
      Pr,
      Nd,
      Pm,
      Sm,
      Eu,
      Gd,
      Tb,
      Dy,
      Ho,
      Er,
      Tm,
      Yb,
      Lu,
      Hf,
      Ta,
      W,
      Re,
      Os,
      Ir,
      Pt,
      Au,
      Hg,
      Tl,
      Pb,
      Bi,
      Po,
      At_Atom,
      Rn,
      Fr,
      Ra,
      Ac,
      Th,
      Pa,
      U);

   package Elem_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Element_Enum);

   type Element_Count is range 0 .. 2**127 - 1;

   type Elem_Count_Array is array (Element_Enum) of Element_Count;

   subtype Elem_Vector is Elem_Vectors.Vector;

   function Identify_Atom_For
     (Sequence : Common.Positive_Vector) return Element_Enum;
   --  returns the atom for a given Count
   --  my start Count corresponded to one of the atoms;
   --  i suspect that's true for everyone, but it's possible that
   --  you would need to Decay it into atoms instead.
   --  that would require more work.

   function Decay (Count : Elem_Count_Array) return Elem_Count_Array;
   --  performs one iteration of decay, according to the rules given in
   --  https://en.wikipedia.org/wiki/Look-and-say_sequence#Cosmological_decay

   function Expanded_Length (Count : Elem_Count_Array) return Element_Count;
   --  returns the length of the Count
   --  when expanded back into a Positive_Vector,
   --  without in fact expanding the vector

end Atomic;
