package body Harriet.UI.Models.Dashboard is

   ----------------------------
   -- Create_Dashboard_Model --
   ----------------------------

   function Create_Dashboard_Model
     (Session : not null access Harriet.Sessions.Root_Harriet_Session'Class)
      return Dashboard_Model
   is
   begin
      return new Root_Dashboard_Model'
        (Session => Harriet.Sessions.Harriet_Session (Session),
         Layout  => <>,
         Watchers => <>,
         Algorithm => Harriet.UI.Layouts.Default_Layout);
   end Create_Dashboard_Model;

   -----------------
   -- Delete_Tile --
   -----------------

   procedure Delete_Tile
     (Model : in out Root_Dashboard_Model'Class;
      Deleted_Tile :  Harriet.UI.Layouts.Tile_Index;
      Replacement_Tile : out Harriet.UI.Layouts.Tile_Count)
   is
      use Harriet.UI.Layouts;
      Column : Layout_Column_Index;
      Row    : Layout_Row_Index;
   begin
      Model.Layout.Find_Tile (Deleted_Tile, Column, Row);
      Model.Algorithm.Delete_Tile (Model.Layout, Deleted_Tile);
      if Column > Model.Layout.Column_Count then
         if Column > 1 then
            Column := Column - 1;
            Row := 1;
         end if;
      elsif Row > Model.Layout.Row_Count (Column) then
         Row := Model.Layout.Row_Count (Column);
      end if;
      if Column <= Model.Layout.Column_Count
        and then Row <= Model.Layout.Row_Count (Column)
      then
         Replacement_Tile := Model.Layout.Get_Tile (Column, Row);
      end if;

   end Delete_Tile;

   --------------
   -- New_Tile --
   --------------

   function New_Tile
     (Model : in out Root_Dashboard_Model'Class)
      return Harriet.UI.Layouts.Tile_Index
   is
   begin
      return Model.Algorithm.New_Tile (Model.Layout);
   end New_Tile;

   ----------------
   -- Swap_Tiles --
   ----------------

   procedure Swap_Tiles
     (Model  : in out Root_Dashboard_Model'Class;
      Tile_1 : Harriet.UI.Layouts.Tile_Index;
      Tile_2 : Harriet.UI.Layouts.Tile_Index)
   is
   begin
      Model.Layout.Swap_Tiles (Tile_1, Tile_2);
   end Swap_Tiles;

end Harriet.UI.Models.Dashboard;
