with Harriet.Real_Images;

with Harriet.Factions;
with Harriet.Ships;

with Harriet.Db.Ship;

package body Harriet.UI.Models.Orbits is

   Name_Column        : constant := 1;
   Owner_Column       : constant := 2;
   Total_Mass_Column  : constant := 3;
   Fuel_Mass_Column   : constant := 4;
   Cargo_Mass_Column  : constant := 5;
   Delta_V_Column     : constant := 6;
   Destination_Column : constant := 7;

   procedure Create_Table
     (Model : Orbiting_Ship_Model);

   ------------
   -- Create --
   ------------

   function Create
     (World : Harriet.Db.World_Reference)
      return Orbiting_Ship_Model
   is
      use Harriet.UI.Models.Tables;
   begin
      return Model : constant Orbiting_Ship_Model :=
        new Root_Orbiting_Ship_Model
      do
         Model.World := World;
         Model.Add_Column ("Name");
         Model.Add_Column ("Owner");
         Model.Add_Column ("Total Mass");
         Model.Add_Column ("Fuel");
         Model.Add_Column ("Cargo");
         Model.Add_Column ("Delta-v");
         Model.Add_Column ("Destination");

         Create_Table (Model);

      end return;
   end Create;

   procedure Create_Table
     (Model : Orbiting_Ship_Model)
   is
   begin
      Model.Clear_Rows;
      for Ship of Harriet.Db.Ship.Select_By_World (Model.World) loop
         declare
            use Harriet.UI.Models.Tables;
            Row   : constant Table_Row_Index :=
                      Model.Add_Row;

         begin

            Model.Set_Cell
              (Row, Name_Column, Ship.Name);
            Model.Set_Cell
              (Row, Owner_Column,
               Harriet.Factions.Get (Ship.Owner).Name);
            Model.Set_Cell
              (Row, Total_Mass_Column,
               Harriet.Real_Images.Approximate_Image
                 (Harriet.Ships.Current_Mass
                      (Harriet.Ships.Get (Ship))));
            Model.Set_Cell (Row, Fuel_Mass_Column, "-");
            Model.Set_Cell (Row, Cargo_Mass_Column, "-");
            Model.Set_Cell (Row, Delta_V_Column, "-");
            Model.Set_Cell (Row, Destination_Column, "-");

         end;
      end loop;
      Model.Clear_Changes;
   end Create_Table;

end Harriet.UI.Models.Orbits;
