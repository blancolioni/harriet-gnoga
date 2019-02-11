with Harriet.Configure.Star_Systems;

with Harriet.Db.Faction;
with Harriet.Db.Generation;
with Harriet.Db.Scenario;
with Harriet.Db.Ship;
with Harriet.Db.Star;
with Harriet.Db.Star_System;
with Harriet.Db.World;

package body Harriet.Star_Systems is

   First_Star_System : Harriet.Db.Star_System_Reference :=
                         Harriet.Db.Null_Star_System_Reference;

   procedure Check_Worlds
     (Star_System : Harriet.Db.Star_System_Reference);

   ------------------
   -- Check_Worlds --
   ------------------

   procedure Check_Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
   is
      Is_Gen : constant Harriet.Db.Is_Generated_Reference :=
                 Harriet.Db.Star_System.Get (Star_System)
                 .Get_Is_Generated_Reference;
      Gen    : constant Harriet.Db.Generation.Generation_Type :=
                 Harriet.Db.Generation.Get_By_Is_Generated
                   (Is_Gen);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Harriet.Configure.Star_Systems.Generate_Star_System
           (Star_System);
         if Gen.Has_Element then
            Gen.Set_Ready (True);
         else
            Harriet.Db.Generation.Create (Is_Gen, True);
         end if;
      end if;
   end Check_Worlds;

   -----------
   -- Claim --
   -----------

   procedure Claim
     (Star_System : Harriet.Db.Star_System_Reference)
   is
   begin
      Harriet.Db.Star_System.Get (Star_System).Set_Claimed (True);
   end Claim;

   -------------
   -- Claimed --
   -------------

   function Claimed
     (Star_System : Harriet.Db.Star_System_Reference)
      return Boolean
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System).Claimed;
   end Claimed;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact
     (Name : String)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.Star_System.First_Reference_By_Name (Name);
   end Find_Exact;

   -----------
   -- First --
   -----------

   function First return Harriet.Db.Star_System_Reference is
      use Harriet.Db;
   begin
      if First_Star_System = Null_Star_System_Reference then
         declare
            Scenario : constant Harriet.Db.Scenario.Scenario_Type :=
                         Harriet.Db.Scenario.Get_By_Active (True);
         begin
            if Scenario.Has_Element then
               First_Star_System := Scenario.Central_System;
            end if;
         end;
      end if;

      if First_Star_System = Null_Star_System_Reference then
         First_Star_System :=
           Harriet.Db.Star_System.First_Reference_By_Top_Record
             (Harriet.Db.R_Star_System);
      end if;

      return First_Star_System;
   end First;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Harriet.Db.Star_System_Reference)
      return Star_System_Type
   is
   begin
      return Star_System_Type'(Reference => Reference);
   end Get;

   ---------------
   -- Get_Ships --
   ---------------

   procedure Get_Ships
     (Star_System : Star_System_Type'Class;
      List        : out Harriet.Ships.Lists.List)
   is
   begin
      for Ship of
        Harriet.Db.Ship.Select_By_Star_System (Star_System.Reference)
      loop
         List.Append (Harriet.Ships.Get (Ship));
      end loop;
   end Get_Ships;

   ---------------------
   -- Has_Star_System --
   ---------------------

   function Has_Star_System
     (Star_System : Star_System_Type'Class)
      return Boolean
   is
      use type Harriet.Db.Star_System_Reference;
   begin
      return Star_System.Reference /= Harriet.Db.Null_Star_System_Reference;
   end Has_Star_System;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Star_System_Type'Class)
      return String
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Harriet.Db.Star_System_Reference)
      return String
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System).Name;
   end Name;

   function Owner
     (Star_System : Star_System_Type'Class)
      return Harriet.Factions.Faction_Type'Class
   is
   begin
      return Harriet.Factions.Get
        (Harriet.Db.Faction.First_Reference_By_Capital_System
           (Star_System.Reference));
   end Owner;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Harriet.Db.Star_System_Reference)
      return Interstellar_Position
   is
      Rec : constant Harriet.Db.Star_System.Star_System_Type :=
              Harriet.Db.Star_System.Get (Star_System);
   begin
      return (Rec.X, Rec.Y, Rec.Z);
   end Position;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Star_System_Type'Class)
      return Interstellar_Position
   is
   begin
      return Position (Star_System.Reference);
   end Position;

   -------------
   -- Primary --
   -------------

   function Primary
     (Star_System : Star_System_Type'Class)
      return Harriet.Stars.Star_Type'Class
   is
   begin
      return Harriet.Stars.Get
        (Harriet.Db.Star.First_Reference_By_Star_System
           (Star_System.Reference));
   end Primary;

   ------------------------
   -- Terrestrial_Worlds --
   ------------------------

   function Terrestrial_Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection
   is
   begin
      return Selection : Harriet.Worlds.World_Selection :=
        Worlds (Star_System)
      do
         Selection.Filter (Harriet.Worlds.Is_Terrestrial'Access);
      end return;
   end Terrestrial_Worlds;

   ------------
   -- Worlds --
   ------------

   function Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection
   is
   begin
      Check_Worlds (Star_System);
      return Selection : Harriet.Worlds.World_Selection do
         for World of
           Harriet.Db.World.Select_By_Star_System
             (Star_System)
         loop
            Selection.Insert (World.Get_World_Reference);
         end loop;
      end return;
   end Worlds;

end Harriet.Star_Systems;
