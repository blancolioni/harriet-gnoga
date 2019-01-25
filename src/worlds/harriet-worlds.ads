private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Db;

with Harriet.Ships.Lists;

package Harriet.Worlds is

   type World_Selection is tagged private;

   function Is_Empty (Selection : World_Selection'Class) return Boolean;

   procedure Clear (Selection : in out World_Selection'Class);
   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Harriet.Db.World_Reference);

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Harriet.Db.World_Reference)
      return Boolean);

   type World_Array is array (Positive range <>) of Harriet.Db.World_Reference;

   function Get_Worlds (Selection : World_Selection'Class) return World_Array;

   function Star_System
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Star_System_Reference;

   function Name
     (World : Harriet.Db.World_Reference)
      return String;

   function Mass
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real;

   function Radius
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real;

   function Climate
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Climate_Reference;

   function Habitability
     (World : Harriet.Db.World_Reference)
      return Unit_Real;

   function Is_Terrestrial
     (World : Harriet.Db.World_Reference)
      return Boolean;

   procedure Scan_Surface
     (World : Harriet.Db.World_Reference;
      Process : not null access
        procedure (Sector : Harriet.Db.World_Sector_Reference));

   procedure Get_Ships
     (World : Harriet.Db.World_Reference;
      Ships : out Harriet.Ships.Lists.List);

   type Sector_Vertex is
      record
         X, Y, Z : Signed_Unit_Real;
      end record;

   function Get_Centre
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex;

   function Get_Terrain
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Terrain_Reference;

   type Sector_Vertex_Array is array (Positive range <>) of Sector_Vertex;

   function Get_Vertices
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex_Array;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.World_Reference, Harriet.Db."=");

   type World_Selection is tagged
      record
         List : World_Lists.List;
      end record;

end Harriet.Worlds;
