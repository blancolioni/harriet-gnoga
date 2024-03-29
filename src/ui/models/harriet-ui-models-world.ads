private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Db;

with Harriet.Worlds;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.World is

   type Root_World_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   function World
     (Model : Root_World_Model'Class)
      return Harriet.Db.World_Reference;

   procedure Scan_Surface
     (Model   : Root_World_Model'Class;
      Process : not null access
        procedure (Sector   : Harriet.Db.World_Sector_Reference;
                   Centre_X : Harriet.Worlds.Sector_Vertex;
                   Boundary : Harriet.Worlds.Sector_Vertex_Array));

   type World_Model is
     access all Root_World_Model'Class;

   function Create
     (World : Harriet.Db.World_Reference)
      return World_Model;

private

   package Sector_Boundary_Holders is
     new Ada.Containers.Indefinite_Holders
       (Harriet.Worlds.Sector_Vertex_Array,
        Harriet.Worlds."=");

   type World_Sector_Type is
      record
         Reference : Harriet.Db.World_Sector_Reference;
         Owner     : Harriet.Db.Faction_Reference;
         Boundary  : Sector_Boundary_Holders.Holder;
         Centre    : Harriet.Worlds.Sector_Vertex;
      end record;

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Sector_Type);

   type Root_World_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         Reference : Harriet.Db.World_Reference;
         Sectors   : World_Sector_Lists.List;
      end record;

   function World
     (Model : Root_World_Model'Class)
      return Harriet.Db.World_Reference
   is (Model.Reference);

end Harriet.UI.Models.World;
