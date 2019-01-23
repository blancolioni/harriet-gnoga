private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Db;

with Harriet.Worlds;

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

   package Sector_Boundary_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Worlds.Sector_Vertex, Harriet.Worlds."=");

   type World_Sector_Type is
      record
         Reference : Harriet.Db.World_Sector_Reference;
         Boundary  : Sector_Boundary_Lists.List;
         Centre    : Harriet.Worlds.Sector_Vertex;
      end record;

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Sector_Type);

   type Root_World_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         Sectors : World_Sector_Lists.List;
      end record;

end Harriet.UI.Models.World;
