with Harriet.Color;

with Harriet.Db;

package Harriet.Factions is

   type Faction_Type is tagged private;

   function Get
     (Reference : Harriet.Db.Faction_Reference)
      return Faction_Type'Class;

   function Has_Element
     (Faction : Faction_Type'Class)
     return Boolean;

   function Name
     (Faction : Faction_Type'Class)
      return String;

   function Color
     (Faction : Faction_Type'Class)
      return Harriet.Color.Harriet_Color;

   function Name
     (Faction : Harriet.Db.Faction_Reference)
      return String;

   function Capital_World
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.World_Reference;

   function Capital_System
     (Faction : Harriet.Db.Faction_Reference)
      return Harriet.Db.Star_System_Reference;

private

   type Faction_Type is tagged
      record
         Reference : Harriet.Db.Faction_Reference;
      end record;

end Harriet.Factions;
