with Harriet.Db;

package Harriet.Ships is

   procedure Create_Ship
     (Owner  : Harriet.Db.Faction_Reference;
      World  : Harriet.Db.World_Reference;
      Design : Harriet.Db.Ship_Design_Reference;
      Name   : String);

end Harriet.Ships;
