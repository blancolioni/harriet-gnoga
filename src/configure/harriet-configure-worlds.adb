with Ada.Text_IO;

with Harriet.Surfaces;

with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.World;
with Harriet.Db.World_Sector;

package body Harriet.Configure.Worlds is

   procedure Save_Surface
     (Surface : Harriet.Surfaces.Surface_Type;
      World   : Harriet.Db.World.World_Type);

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (World : Harriet.Db.World_Reference)
   is
      use Harriet.Db;
      Required_Depth : Natural;
      Rec            : constant Harriet.Db.World.World_Type :=
                         Harriet.Db.World.Get (World);
   begin
      case Rec.Category is
         when Asteroid =>
            Required_Depth := 1;
         when Dwarf =>
            Required_Depth := 2;
         when Terrestrial =>
            Required_Depth := 3;
         when Super_Terrestrial =>
            Required_Depth := 4;
         when Sub_Jovian | Jovian | Super_Jovian =>
            Required_Depth := 0;
      end case;

      if Required_Depth = 0 then
         return;
      end if;

      declare
         Surface   : Harriet.Surfaces.Root_Surface_Type;
      begin
         Ada.Text_IO.Put_Line
           (Rec.Name & ": creating surface of depth" & Required_Depth'Image);
         Surface.Create (Required_Depth);

         Ada.Text_IO.Put_Line
           (Rec.Name & ": saving"
            & Harriet.Surfaces.Surface_Tile_Count'Image
              (Surface.Tile_Count)
            & " tiles to database");

         Save_Surface (Surface, Rec);

         Ada.Text_IO.Put_Line
           (Rec.Name & ": done");

      end;

   end Generate_Surface;

   ------------------
   -- Save_Surface --
   ------------------

   procedure Save_Surface
     (Surface   : Harriet.Surfaces.Surface_Type;
      World     : Harriet.Db.World.World_Type)
   is
      Tile_Refs : array (1 .. Surface.Tile_Count)
        of Harriet.Db.Sector_Reference;
   begin
      for Tile_Index in 1 .. Surface.Tile_Count loop
         declare
            use Harriet.Surfaces;
            Sector : constant Harriet.Db.World_Sector.World_Sector_Type :=
                       Harriet.Db.World_Sector.Create;
            Centre : constant Vector_3 := Surface.Tile_Centre (Tile_Index);
         begin
            Sector.Set_World (World.Reference);
            Sector.Set_Surface (World.Reference);
            Sector.Set_X (Centre (1));
            Sector.Set_Y (Centre (2));
            Sector.Set_Z (Centre (3));

            for Point of Surface.Tile_Boundary (Tile_Index) loop
               Harriet.Db.Sector_Vertex.Create
                 (Sector        => Sector.Reference,
                  X             => Point (1),
                  Y             => Point (2),
                  Z             => Point (3));
            end loop;

            Tile_Refs (Tile_Index) := Sector.Reference;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         for I in 1 .. Surface.Neighbour_Count (Tile_Index) loop
            Harriet.Db.Sector_Neighbour.Create
              (Sector    => Tile_Refs (Tile_Index),
               Neighbour => Tile_Refs (Surface.Neighbour (Tile_Index, I)));
         end loop;
      end loop;

   end Save_Surface;

end Harriet.Configure.Worlds;
