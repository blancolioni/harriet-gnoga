with Harriet.Db.Terrain;

package body Harriet.Terrain is

   -----------
   -- Color --
   -----------

   function Color
     (Terrain : Harriet.Db.Terrain_Reference)
      return Harriet.Color.Harriet_Color
   is
      Rec : Harriet.Db.Terrain.Terrain_Type :=
              Harriet.Db.Terrain.Get (Terrain);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

end Harriet.Terrain;
