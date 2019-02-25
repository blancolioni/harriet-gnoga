with Harriet.Db.Faction;
with Harriet.Db.World;

package body Harriet.Factions is

   --------------------
   -- Capital_System --
   --------------------

   function Capital_System
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.World.Get (Capital_World (Faction)).Star_System;
   end Capital_System;

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.World_Reference
   is
   begin
      return Harriet.Db.Faction.Get (Faction).Capital_World;
   end Capital_World;

   -----------
   -- Color --
   -----------

   function Color
     (Faction : Faction_Type'Class)
      return Harriet.Color.Harriet_Color
   is
      Rec : constant Harriet.Db.Faction.Faction_Type :=
              Harriet.Db.Faction.Get (Faction.Reference);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Harriet.Db.Faction_Reference)
      return Faction_Type'Class
   is
   begin
      return Faction_Type'(Reference => Reference);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Harriet.Db.Owner_Reference)
      return Faction_Type'Class
   is
      Faction : constant Harriet.Db.Faction.Faction_Type :=
                  Harriet.Db.Faction.Get_Faction (Reference);
   begin
      return Get (Faction.Get_Faction_Reference);
   end Get;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Faction : Faction_Type'Class)
      return Boolean
   is
      use type Harriet.Db.Faction_Reference;
   begin
      return Faction.Reference /= Harriet.Db.Null_Faction_Reference;
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Faction_Type'Class)
      return String
   is
   begin
      return Harriet.Db.Faction.Get (Faction.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Harriet.Db.Faction_Reference)
      return String
   is
   begin
      return Harriet.Db.Faction.Get (Faction).Name;
   end Name;

end Harriet.Factions;
