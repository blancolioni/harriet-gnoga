with Harriet.Quantities;

with Harriet.Db.Pop;

package Harriet.Pops is

   function New_Empty_Pop
     (Faction : Harriet.Db.Faction_Reference;
      Group   : Harriet.Db.Pop_Group_Reference;
      World   : Harriet.Db.World_Reference;
      Sector  : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Pop.Pop_Type;

   procedure Move_Pops
     (From     : Harriet.Db.Pop.Pop_Type;
      To       : Harriet.Db.Pop.Pop_Type;
      Quantity : Harriet.Quantities.Quantity_Type);

end Harriet.Pops;
