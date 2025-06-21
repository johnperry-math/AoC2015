--  Advent of Code 2015
--  John Perry
--
--  help a kid buy the right equipment to getpast the boss on a video game

pragma Ada_2022;

with Ada.Text_IO;

procedure Day21 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   Debugging : constant Boolean := False;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  items you can buy

   type Items_Enum is
     (Dagger,
      Shortsword,
      Warhammer,
      Longsword,
      Greataxe,
      Leather,
      Chainmail,
      Splintmail,
      Bandedmail,
      Platemail,
      Damage_1,
      Damage_2,
      Damage_3,
      Defense_1,
      Defense_2,
      Defense_3);

   subtype Weapons_Enum is Items_Enum range Dagger .. Greataxe;

   subtype Armor_Enum is Items_Enum range Leather .. Platemail;

   subtype Rings_Enum is Items_Enum range Damage_1 .. Defense_3;

   type Item_Record is record
      Cost, Damage, Armor : Natural;
   end record;

   Items : constant array (Items_Enum) of Item_Record :=
     [Dagger     => (Cost => 8, Damage => 4, Armor => 0),
      Shortsword => (Cost => 10, Damage => 5, Armor => 0),
      Warhammer  => (Cost => 25, Damage => 6, Armor => 0),
      Longsword  => (Cost => 40, Damage => 7, Armor => 0),
      Greataxe   => (Cost => 74, Damage => 8, Armor => 0),
      Leather    => (Cost => 13, Damage => 0, Armor => 1),
      Chainmail  => (Cost => 31, Damage => 0, Armor => 2),
      Splintmail => (Cost => 53, Damage => 0, Armor => 3),
      Bandedmail => (Cost => 75, Damage => 0, Armor => 4),
      Platemail  => (Cost => 102, Damage => 0, Armor => 5),
      Damage_1   => (Cost => 25, Damage => 1, Armor => 0),
      Damage_2   => (Cost => 50, Damage => 2, Armor => 0),
      Damage_3   => (Cost => 100, Damage => 3, Armor => 0),
      Defense_1  => (Cost => 20, Damage => 0, Armor => 1),
      Defense_2  => (Cost => 40, Damage => 0, Armor => 2),
      Defense_3  => (Cost => 80, Damage => 0, Armor => 3)];

   --  SUBSECTION
   --  As_Configured data

   type Inventory_Array is array (Items_Enum) of Boolean;

   type Player_Record is record
      Damage        : Natural;
      Armor         : Natural;
      Hit_Points    : Natural := 100;
      Inventory_Has : Inventory_Array := [others => False];
   end record;

   Boss : Player_Record;

   --  SECTION
   --  I/O

   procedure Read_Boss is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         HP_Line     : constant String := IO.Get_Line (Input);
         Damage_Line : constant String := IO.Get_Line (Input);
         Armor_Line  : constant String := IO.Get_Line (Input);
         Position    : Positive;
      begin
         Positive_IO.Get
           (HP_Line (13 .. HP_Line'Last), Boss.Hit_Points, Position);
         Positive_IO.Get
           (Damage_Line (9 .. Damage_Line'Last), Boss.Damage, Position);
         Positive_IO.Get
           (Armor_Line (8 .. Armor_Line'Last), Boss.Armor, Position);
      end;
      IO.Close (Input);
   end Read_Boss;

   procedure Put (Player : Player_Record) is
      --  useful for debugging
   begin
      IO.Put (Player.Armor'Image & ";" & Player.Damage'Image & "; ");
      for Item in Items_Enum loop
         if Player.Inventory_Has (Item) then
            IO.Put (Item'Image & " ");
         end if;
      end loop;
   end Put;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  combat

   function Damage_Inflicted
     (Attacker, Defender : Player_Record) return Natural
   is (Integer'Max (1, Attacker.Damage - Defender.Armor));

   function Human_Wins (Human : Player_Record) return Boolean is
      --  true iff the human is the last one with positive hit points
      Human_HP : Natural := Human.Hit_Points;
      Boss_HP  : Natural := Boss.Hit_Points;
      Turns    : Natural := 0;
   begin

      --  fight!
      while Human_HP > 0 and then Boss_HP > 0 loop
         Turns := @ + 1;
         Boss_HP := Integer'Max (0, @ - Damage_Inflicted (Human, Boss));
         if Boss_HP > 0 then
            Human_HP := Integer'Max (0, @ - Damage_Inflicted (Boss, Human));
         end if;
      end loop;

      --  debug!
      if Debugging then
         IO.Put_Line
           ("         after"
            & Turns'Image
            & " turns, result: "
            & Human_HP'Image);
      end if;

      return Human_HP > 0;
   end Human_Wins;

   --  SUBSECTION
   --  statistics based on inventory

   function Cost_To_Equip (Player : Player_Record) return Natural is
      --  sum of costs of items in As_Configured's inventory
      Result : Natural := 0;
   begin
      for Item in Items_Enum loop
         if Player.Inventory_Has (Item) then
            Result := @ + Items (Item).Cost;
         end if;
      end loop;
      return Result;
   end Cost_To_Equip;

   procedure Configure_Armor_And_Damage (Player : in out Player_Record) is
      --  fills in As_Configured's Armor and Damage fields based on inventory
      Armor_Value, Damage_Value : Natural := 0;
   begin

      --  armor: armor
      for Armor in Armor_Enum loop
         if Player.Inventory_Has (Armor) then
            Armor_Value := @ + Items (Armor).Armor;
         end if;
      end loop;

      -- weapons: damage
      for Weapon in Weapons_Enum loop
         if Player.Inventory_Has (Weapon) then
            Damage_Value := @ + Items (Weapon).Damage;
         end if;
      end loop;

      --  rings: both armor and damage
      for Ring in Rings_Enum loop
         if Player.Inventory_Has (Ring) then
            Armor_Value := @ + Items (Ring).Armor;
            Damage_Value := @ + Items (Ring).Damage;
         end if;
      end loop;

      Player.Armor := Armor_Value;
      Player.Damage := Damage_Value;

      if Debugging then
         IO.Put ("         ");
         Put (Player);
         IO.New_Line;
      end if;

   end Configure_Armor_And_Damage;

   --  SUBSECTION
   --  computing costs

   type Cost_Record is record
      Least_Cost_To_Win : Natural := Natural'Last;
      Most_Cost_To_Lose : Natural := Natural'First;
   end record;

   function Adjust (Left, Right : Cost_Record) return Cost_Record
   is (Cost_Record'
         (Least_Cost_To_Win =>
            Natural'Min (Left.Least_Cost_To_Win, Right.Least_Cost_To_Win),
          Most_Cost_To_Lose =>
            Natural'Max (Left.Most_Cost_To_Lose, Right.Most_Cost_To_Lose)));
   --  minimizes cost to win and maximizes cost to lose

   function Costs_With_Rings
     (Player : Player_Record; Rings : Natural) return Cost_Record
   is
      --  returns the costs associated with equipping Player
      --  with the indicated number of rings
      Result        : Cost_Record;
      As_Configured : Player_Record := Player;
   begin

      if Debugging then
         IO.Put_Line ("      Rings available:" & Rings'Image);
      end if;

      if Rings = 0 then
         Configure_Armor_And_Damage (As_Configured);
         declare
            Cost : constant Natural := Cost_To_Equip (As_Configured);
         begin
            if Human_Wins (As_Configured) then
               Result.Least_Cost_To_Win := Cost;
            else
               Result.Most_Cost_To_Lose := Cost;
            end if;
         end;

      else
         for Ring in Rings_Enum when not As_Configured.Inventory_Has (Ring)
         loop
            As_Configured.Inventory_Has (Ring) := True;
            Result := Adjust (@, Costs_With_Rings (As_Configured, Rings - 1));
            As_Configured.Inventory_Has (Ring) := False;
         end loop;

      end if;

      return Result;

   end Costs_With_Rings;

   function Costs_With_Armor
     (Player : Player_Record; Num_Armor : Natural; Num_Rings : Natural)
      return Cost_Record
   is
      --  returns the costs associated with equipping Player
      --  with the indicated number of armor and rings
      Result        : Cost_Record;
      As_Configured : Player_Record := Player;
   begin

      if Debugging then
         IO.Put_Line
           ("   Armor, rings available:" & Num_Armor'Image & Num_Rings'Image);
      end if;

      if Num_Armor = 0 then
         Result := Costs_With_Rings (As_Configured, Num_Rings);

      else
         for Armor in Armor_Enum when not As_Configured.Inventory_Has (Armor)
         loop
            As_Configured.Inventory_Has (Armor) := True;
            Result :=
              Adjust
                (@,
                 Costs_With_Armor (As_Configured, Num_Armor - 1, Num_Rings));
            As_Configured.Inventory_Has (Armor) := False;
         end loop;
      end if;

      return Result;

   end Costs_With_Armor;

   function Solution return Cost_Record is
      --  solves both parts in one go!
      Result : Cost_Record;
      Player : Player_Record;
   begin

      for Weapon in Weapons_Enum loop

         Player.Inventory_Has (Weapon) := True;

         if Debugging then
            IO.Put_Line (Weapon'Image);
         end if;

         for Num_Armor in 0 .. 1 loop
            for Num_Rings in 0 .. 2 loop
               Result :=
                 Adjust (@, Costs_With_Armor (Player, Num_Armor, Num_Rings));
            end loop;
         end loop;

         Player.Inventory_Has (Weapon) := False;

      end loop;

      return Result;

   end Solution;

begin
   Read_Boss;
   declare
      Costs : constant Cost_Record := Solution;
   begin
      IO.Put_Line
        ("The least expensive winning inventory costs"
         & Costs.Least_Cost_To_Win'Image);
      IO.Put_Line
        ("The most expensive losing inventory costs"
         & Costs.Most_Cost_To_Lose'Image);
   end;
end Day21;
