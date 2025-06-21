--  Advent of Code 2015
--  John Perry
--
--  now help the same kid with a different game

pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Day22 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   Debugging : Boolean := False;

   --  SECTION
   --  gobal types and variables

   --  SUBSECTION
   --  spells

   type Spells_Enum is (Magic_Missile, Drain, Shield, Poison, Recharge);

   package Spells_IO is new IO.Enumeration_IO (Enum => Spells_Enum);

   Mana_Cost : constant array (Spells_Enum) of Positive :=
     [Magic_Missile => 53,
      Drain         => 73,
      Shield        => 113,
      Poison        => 173,
      Recharge      => 229];

   Effect_Duration : constant array (Spells_Enum) of Natural :=
     [Magic_Missile => 1, Drain => 1, Shield => 6, Poison => 6, Recharge => 5];

   Damage_Bonus : constant array (Spells_Enum) of Natural :=
     [Magic_Missile => 0, Drain => 0, Shield => 0, Poison => 3, Recharge => 0];

   Healing_Bonus : constant array (Spells_Enum) of Natural :=
     [Magic_Missile => 0, Drain => 2, Shield => 0, Poison => 0, Recharge => 0];

   Mana_Bonus : constant array (Spells_Enum) of Natural :=
     [Magic_Missile => 0,
      Drain         => 0,
      Shield        => 0,
      Poison        => 0,
      Recharge      => 101];

   Armor_Bonus : constant array (Spells_Enum) of Natural :=
     [Magic_Missile => 0, Drain => 0, Shield => 7, Poison => 0, Recharge => 0];

   type Spell_Record (Kind : Spells_Enum) is record
      Time_Remaining  : Natural := 1;
      Armor_Benefit   : Natural := 0;
      Damage_Benefit  : Natural := 0;
      Healing_Benefit : Natural := 0;
      Mana_Benefit    : Natural := 0;
   end record;

   package Spell_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Spell_Record);

   subtype Spell_Vector is Spell_Vectors.Vector;

   --  SUBSECTION
   --  players

   type Player_Record is record
      Hit_Points    : Natural := 50;
      Armor, Damage : Natural := 0;
      Mana          : Natural := 500;
      Spells_Active : Spell_Vector;
   end record;

   Boss : Player_Record;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  input

   procedure Read_Boss is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         Line_1   : constant String := IO.Get_Line (Input);
         Line_2   : constant String := IO.Get_Line (Input);
         Position : Positive;
      begin
         Positive_IO.Get
           (Line_1 (13 .. Line_1'Last), Boss.Hit_Points, Position);
         Positive_IO.Get (Line_2 (9 .. Line_2'Last), Boss.Damage, Position);
      end;
      IO.Close (Input);
      Boss.Mana := 0;
   end Read_Boss;

   --  SUBSECTION
   --  output

   procedure Put (Spells : Spell_Vector) is
   begin
      IO.Put ('(');
      for Spell of Spells when Spell.Kind not in Magic_Missile .. Drain loop
         IO.Put ('(');
         Positive_IO.Put (Spell.Time_Remaining, 0);
         IO.Put (", Spell(name='");
         Spells_IO.Put (Spell.Kind, 0, IO.Lower_Case);
         IO.Put ("', cost=");
         Positive_IO.Put (Mana_Cost (Spell.Kind), 0);
         IO.Put (')');
      end loop;
      IO.Put (')');
   end Put;

   procedure Put (Player : Player_Record) is
   begin
      IO.Put_Line
        ("   HP"
         & Player.Hit_Points'Image
         & " A"
         & Player.Armor'Image
         & " D"
         & Player.Damage'Image
         & " M"
         & Player.Mana'Image);
      for Spell of Player.Spells_Active loop
         IO.Put_Line
           ("         " & Spell.Kind'Image & Spell.Time_Remaining'Image);
      end loop;
   end Put;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  tracking the state of play

   type State_Record is record
      Boss_State   : Player_Record;
      Player_State : Player_Record;
      Mana_Spent   : Natural := 0;
   end record;

   package State_Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => State_Record);

   function Get_Priority (State : State_Record) return Natural
   is (State.Mana_Spent);

   package State_Queues is new
     Ada.Containers.Unbounded_Priority_Queues
       (Queue_Interfaces => State_Queue_Interfaces,
        Queue_Priority   => Positive,
        Before           => "<");

   subtype State_Queue is State_Queues.Queue;

   --  SUBSECTION
   --  magic effects

   procedure Configure_Player (Player : in out Player_Record) is
      --  set up player's hit points, damage, armor, and mana
      --  according to spells in use
      Armor_Value,  --
      Damage_Value, --
      Mana_Value,   --
      Healing_Value : Natural := 0;
      New_Spells    : Spell_Vector;
      In_Use        : array (Spells_Enum) of Boolean := [others => False];
   begin

      --  loop through spells and sum the bonuses,
      --  dropping the ones that expire after this usage
      for Spell of Player.Spells_Active loop
         if not In_Use (Spell.Kind) then
            if Debugging then
               IO.Put_Line ("      applying " & Spell.Kind'Image);
            end if;
            In_Use (Spell.Kind) := True;
            Armor_Value := @ + Armor_Bonus (Spell.Kind);
            Damage_Value := @ + Damage_Bonus (Spell.Kind);
            Mana_Value := @ + Mana_Bonus (Spell.Kind);
            Healing_Value := @ + Healing_Bonus (Spell.Kind);
            Spell.Time_Remaining := @ - 1;
         end if;
         if Spell.Time_Remaining > 0 then
            New_Spells.Append (Spell);
         end if;
      end loop;

      --  apply the bonuses
      Player.Spells_Active := New_Spells;
      Player.Hit_Points := @ + Healing_Value;
      Player.Damage := Damage_Value;
      Player.Armor := Armor_Value;
      Player.Mana := @ + Mana_Value;

   end Configure_Player;

   procedure Apply_Effects (Player, Boss : in out Player_Record) is
      --  apply's Player's effects to Boss and to Player
   begin
      Configure_Player (Player);
      Boss.Hit_Points :=
        Integer'Max (0, @ - Integer'Max (0, Player.Damage - Boss.Armor));
   end Apply_Effects;

   --  SUSBECTION
   --  attacks that do not involve effects

   procedure Attack (Defender : in out Player_Record; Amount : Positive) is
      --  applies the Amount of damage to the Defener,
      --  mitigated by any armor
      --
      --  note: i really lucked out; at one point I had 0 instead of 1
      --  as the minimum amount of damage one could inflict,
      --  and somehow it still gave the correct answer
   begin
      Defender.Hit_Points :=
        Integer'Max (0, @ - Integer'Max (1, Amount - Defender.Armor));
   end Attack;

   --  SUBSECTION
   --  solution to parts 1 and 2

   type Part_Enum is (First, Second);

   function Solution (Part : Part_Enum) return Natural is
      --  performs a breadth-first search through all possible states,
      --  prioritizing states of minimal mana usage
      Queue  : State_Queue;
      State  : State_Record;
      Result : Positive := Positive'Last;
      Turns  : Natural := 0;
   begin

      State.Boss_State := Boss;
      Queue.Enqueue (State);

      while Natural (Queue.Current_Use) > 0 loop
         Turns := @ + 1;
         Queue.Dequeue (State);

         if State.Mana_Spent < Result then

            if Part = Second then
               --  in the second part, the player always loses a hit point
               --  at the start of his turn
               State.Player_State.Hit_Points := @ - 1;
            end if;

            if State.Player_State.Hit_Points > 0 then

               Apply_Effects (State.Player_State, State.Boss_State);

               if Debugging then
                  Put (State.Player_State);
                  Put (State.Boss_State);
               end if;

               --  cast a spell that is:
               --  * not currently in use;
               --  * affordable; and
               --  * would not raise mana usage above known best result
               for Spell_Available
                 in Spells_Enum
                 when (for all Spell of State.Player_State.Spells_Active
                       => Spell.Kind /= Spell_Available
                          or else Spell.Time_Remaining = 1)
                 and then Mana_Cost (Spell_Available)
                          <= State.Player_State.Mana
                 and then Mana_Cost (Spell_Available) + State.Mana_Spent
                          < Result
               loop

                  declare
                     New_State : State_Record := State;
                  begin

                     if Debugging then
                        IO.Put_Line ("   applying " & Spell_Available'Image);
                     end if;

                     --  Magic_Missile and Drain don't have effects
                     if Spell_Available = Magic_Missile then
                        Attack (New_State.Boss_State, 4);

                     elsif Spell_Available = Drain then
                        New_State.Boss_State.Hit_Points := @ - 2;
                        New_State.Player_State.Hit_Points := @ + 2;

                     else
                        New_State.Player_State.Spells_Active.Append
                          (Spell_Record'
                             (Kind            => Spell_Available,
                              Time_Remaining  =>
                                Effect_Duration (Spell_Available),
                              Armor_Benefit   => Armor_Bonus (Spell_Available),
                              Damage_Benefit  =>
                                Damage_Bonus (Spell_Available),
                              Healing_Benefit =>
                                Healing_Bonus (Spell_Available),
                              Mana_Benefit    =>
                                Mana_Bonus (Spell_Available)));
                     end if;

                     --  adjust state for new spell
                     New_State.Player_State.Mana :=
                       @ - Mana_Cost (Spell_Available);
                     New_State.Mana_Spent := @ + Mana_Cost (Spell_Available);

                     if New_State.Boss_State.Hit_Points = 0 then
                        Result := Positive'Min (@, New_State.Mana_Spent);

                     else
                        if Debugging then
                           IO.Put_Line ("   boss attacks");
                        end if;
                        Apply_Effects
                          (New_State.Player_State, New_State.Boss_State);
                        Attack
                          (New_State.Player_State,
                           New_State.Boss_State.Damage);
                        if New_State.Player_State.Hit_Points > 0 then
                           if Debugging then
                              IO.Put ("   player ");
                              Put (New_State.Player_State);
                              IO.Put ("   boss ");
                              Put (New_State.Boss_State);
                           end if;
                           Queue.Enqueue (New_State);
                        end if;
                     end if;

                  end;

               end loop;
            end if;
         end if;
      end loop;

      return Result;

   end Solution;

   --  SECTION
   --  parts 1 and 2

begin
   Read_Boss;
   IO.Put_Line
     ("The least amount of mana it will cost to win is"
      & Solution (First)'Image);
   IO.Put_Line ("On the hard level, it's" & Solution (Second)'Image);
end Day22;
