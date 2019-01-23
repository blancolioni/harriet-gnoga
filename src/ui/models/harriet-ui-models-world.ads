with Harriet.Db;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.World is

   type Root_World_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   type World_Model is
     access all Root_World_Model'Class;

   function Create
     (World : Harriet.Db.World_Reference)
      return World_Model;

private

   type Root_World_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         null;
      end record;

end Harriet.UI.Models.World;
