with Harriet.Sessions;
with Harriet.UI.Layouts;

package Harriet.UI.Models.Dashboard is

   type Root_Dashboard_Model is
     new Root_Harriet_Model with private;

   function Layout
     (Model : Root_Dashboard_Model'Class)
      return Harriet.UI.Layouts.Layout_Type;

   function New_Tile
     (Model : in out Root_Dashboard_Model'Class)
      return Harriet.UI.Layouts.Tile_Index;

   procedure Delete_Tile
     (Model : in out Root_Dashboard_Model'Class;
      Deleted_Tile :  Harriet.UI.Layouts.Tile_Index;
      Replacement_Tile : out Harriet.UI.Layouts.Tile_Count);

   procedure Swap_Tiles
     (Model  : in out Root_Dashboard_Model'Class;
      Tile_1 : Harriet.UI.Layouts.Tile_Index;
      Tile_2 : Harriet.UI.Layouts.Tile_Index);

   type Dashboard_Model is access all Root_Dashboard_Model'Class;

   function Create_Dashboard_Model
     (Session : not null access Harriet.Sessions.Root_Harriet_Session'Class)
      return Dashboard_Model;

private

   type Root_Dashboard_Model is
     new Root_Harriet_Model with
      record
         Session   : Harriet.Sessions.Harriet_Session;
         Layout    : Harriet.UI.Layouts.Layout_Type;
         Algorithm : Harriet.UI.Layouts.Layout_Algorithm;
      end record;

   overriding function Title
     (Model : Root_Dashboard_Model)
      return String
   is (Model.Session.User_Name);

   function Layout
     (Model : Root_Dashboard_Model'Class)
      return Harriet.UI.Layouts.Layout_Type
   is (Model.Layout);

end Harriet.UI.Models.Dashboard;
