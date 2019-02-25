with Harriet.Db;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.Orbits is

   type Root_Orbiting_Ship_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   type Orbiting_Ship_Model is access all Root_Orbiting_Ship_Model'Class;

   function Create
     (World : Harriet.Db.World_Reference)
      return Orbiting_Ship_Model;

private

   type Root_Orbiting_Ship_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         World : Harriet.Db.World_Reference;
      end record;

end Harriet.UI.Models.Orbits;
