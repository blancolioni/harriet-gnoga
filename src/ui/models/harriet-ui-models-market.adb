with Ada.Text_IO;

with Harriet.Commodities;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Ask_Offer;
with Harriet.Db.Bid_Offer;
with Harriet.Db.Commodity;

package body Harriet.UI.Models.Market is

   type Market_Model_Data is
     new Harriet.Markets.Market_Data with
      record
         Model : Market_Model;
      end record;

   procedure Create_Table
     (Model : in out Root_Market_Model'Class);

   procedure On_Market_Offer
     (Data      : Harriet.Markets.Market_Data'Class;
      Offer     : Harriet.Db.Offer_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   procedure On_Market_Transaction
     (Data      : Harriet.Markets.Market_Data'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is null;

   ------------
   -- Create --
   ------------

   function Create
     (Market  : Harriet.Db.Market_Reference)
      return Market_Model
   is
      use Harriet.UI.Models.Tables;
   begin
      return Model : constant Market_Model := new Root_Market_Model do
         Model.Reference := Market;
         Model.Add_Column ("commodity");
         Model.Add_Column ("bid");
         Model.Add_Column ("ask");
         Model.Add_Column ("demand");
         Model.Add_Column ("supply");

         Model.Create_Table;

         declare
            Data : constant Market_Model_Data :=
                     (Model => Model);
         begin
            Model.Market_Watcher_Id :=
              Harriet.Markets.Add_Market_Watcher
                (Market         => Market,
                 Data           => Data,
                 On_Offer       => On_Market_Offer'Access,
                 On_Transaction => On_Market_Transaction'Access);
         end;

      end return;
   end Create;

   ------------------
   -- Create_Table --
   ------------------

   procedure Create_Table
     (Model : in out Root_Market_Model'Class)
   is
   begin
      Model.Clear_Rows;
      for Commodity of Harriet.Db.Commodity.Scan_By_Tag loop
         declare
            Row   : constant Harriet.UI.Models.Tables.Table_Row_Index :=
                      Model.Add_Row;
         begin
            Model.Commodity_Row.Replace_Element
              (Commodity.Get_Commodity_Reference, Row);

            Model.Set_Cell
              (Row, 1,
               Harriet.Commodities.Local_Name
                 (Commodity.Get_Commodity_Reference));
            Model.Set_Cell
              (Row, 2, "-");
            Model.Set_Cell
              (Row, 3, "-");

            for Offer of
              Harriet.Db.Bid_Offer
                .Select_Market_Priority_Bounded_By_Priority
                  (Model.Reference, Commodity.Get_Commodity_Reference,
                   0.0, Real'Last)
            loop
               Model.Set_Cell
                 (Row, 2,
                  Harriet.Money.Show (Offer.Price));
               exit;
            end loop;
            for Offer of
              Harriet.Db.Ask_Offer
                .Select_Market_Priority_Bounded_By_Priority
                  (Model.Reference, Commodity.Get_Commodity_Reference,
                   0.0, Real'Last)
            loop
               Model.Set_Cell
                 (Row, 3,
                  Harriet.Money.Show (Offer.Price));
               exit;
            end loop;
         end;
      end loop;
      Model.Clear_Changes;
   end Create_Table;

   ---------------------
   -- On_Market_Offer --
   ---------------------

   procedure On_Market_Offer
     (Data      : Harriet.Markets.Market_Data'Class;
      Offer     : Harriet.Db.Offer_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
      use Harriet.UI.Models.Tables;
--        pragma Unreferenced (Quantity);
      Model : constant Market_Model :=
                Market_Model_Data (Data).Model;
      Row   : constant Table_Row_Index :=
                Model.Commodity_Row.Element (Commodity);
   begin
      case Offer is
         when Harriet.Db.Ask =>
            Model.Set_Cell (Row, 3, Harriet.Money.Show (Price));
            Ada.Text_IO.Put_Line
              (Harriet.Commodities.Local_Name (Commodity)
               & ": ask " & Harriet.Money.Show (Price)
               & " for " & Harriet.Quantities.Show (Quantity));
         when Harriet.Db.Bid =>
            Model.Set_Cell (Row, 2, Harriet.Money.Show (Price));
            Ada.Text_IO.Put_Line
              (Harriet.Commodities.Local_Name (Commodity)
               & ": bid " & Harriet.Money.Show (Price)
               & " for " & Harriet.Quantities.Show (Quantity));
      end case;
      Model.Notify_Changed;
   end On_Market_Offer;

end Harriet.UI.Models.Market;
