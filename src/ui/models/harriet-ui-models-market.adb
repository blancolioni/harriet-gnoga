with Harriet.Calendar;
with Harriet.Commodities;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Historical_Ask;
with Harriet.Db.Historical_Bid;
with Harriet.Db.Commodity;
with Harriet.Db.Transaction;

package body Harriet.UI.Models.Market is

   type Market_Model_Data is
     new Harriet.Markets.Market_Data with
      record
         Model : Market_Model;
      end record;

   procedure Create_Table
     (Model : Market_Model);

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
      Price     : Harriet.Money.Price_Type);

   Commodity_Column     : constant := 1;
   Bid_Column           : constant := 2;
   Ask_Column           : constant := 3;
   Last_Trade_Column    : constant := 4;
   Last_Price_Column    : constant := 5;
   Last_Quantity_Column : constant := 6;
   Demand_Column        : constant := 7;
   Supply_Column        : constant := 8;

   procedure Update_Commodity
     (Model     : Market_Model;
      Commodity : Harriet.Db.Commodity_Reference;
      Row       : Harriet.UI.Models.Tables.Table_Row_Index);

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
         Model.Add_Column ("last-trade");
         Model.Add_Column ("last-price");
         Model.Add_Column ("last-traded");
         Model.Add_Column ("demand");
         Model.Add_Column ("supply");

         Create_Table (Model);

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
     (Model : Market_Model)
   is
   begin
      Model.Clear_Rows;
      for Commodity of Harriet.Db.Commodity.Scan_By_Tag loop
         declare
            use Harriet.UI.Models.Tables;
            Row   : constant Table_Row_Index :=
                      Model.Add_Row;
         begin
            Model.Commodity_Row.Replace_Element
              (Commodity.Get_Commodity_Reference, Row);

            Model.Set_Cell
              (Row, Commodity_Column,
               Harriet.Commodities.Local_Name
                 (Commodity.Get_Commodity_Reference));
            for I in Table_Column_Index range Bid_Column .. Supply_Column loop
               Model.Set_Cell
                 (Row, I, "-");
            end loop;

            Update_Commodity (Model, Commodity.Get_Commodity_Reference, Row);

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
      pragma Unreferenced (Offer, Quantity, Price);
      use Harriet.UI.Models.Tables;
      Model : constant Market_Model :=
                Market_Model_Data (Data).Model;
      Row   : constant Table_Row_Index :=
                Model.Commodity_Row.Element (Commodity);
   begin
      Update_Commodity (Model, Commodity, Row);
      Model.Notify_Changed;
   end On_Market_Offer;

   ---------------------------
   -- On_Market_Transaction --
   ---------------------------

   procedure On_Market_Transaction
     (Data      : Harriet.Markets.Market_Data'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
      pragma Unreferenced (Quantity, Price);
      use Harriet.UI.Models.Tables;
      Model : constant Market_Model :=
                Market_Model_Data (Data).Model;
      Row   : constant Table_Row_Index :=
                Model.Commodity_Row.Element (Commodity);
   begin
      Update_Commodity (Model, Commodity, Row);
      Model.Notify_Changed;
   end On_Market_Transaction;

   ----------------------
   -- Update_Commodity --
   ----------------------

   procedure Update_Commodity
     (Model     : Market_Model;
      Commodity : Harriet.Db.Commodity_Reference;
      Row       : Harriet.UI.Models.Tables.Table_Row_Index)
   is
      use Harriet.Money, Harriet.Quantities;
      use type Harriet.Calendar.Time;
      Price    : Price_Type := Zero;
      Quantity : Quantity_Type := Zero;
      Now      : constant Harriet.Calendar.Time := Harriet.Calendar.Clock;
      Date     : Harriet.Calendar.Time := Now;
   begin
      for Offer of
        Harriet.Db.Historical_Bid
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Harriet.Calendar.Days (1), Now)
      loop
         Price := Offer.Price;
         Quantity := Quantity + Offer.Quantity;
      end loop;

      Model.Set_Cell
        (Row, Bid_Column, Harriet.Money.Show (Price));
      Model.Set_Cell (Row, Demand_Column, Show (Quantity));

      Price := Zero;
      Quantity := Zero;

      for Offer of
        Harriet.Db.Historical_Ask
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Harriet.Calendar.Days (1), Now)
      loop
         Price := Offer.Price;
         Quantity := Quantity + Offer.Quantity;
      end loop;

      Model.Set_Cell
        (Row, Ask_Column, Harriet.Money.Show (Price));
      Model.Set_Cell (Row, Supply_Column, Show (Quantity));

      for Transaction of
        Harriet.Db.Transaction
          .Select_Transaction_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Harriet.Calendar.Days (7), Now)
      loop
         Price := Transaction.Price;
         Quantity := Transaction.Quantity;
         Date := Transaction.Time_Stamp;
      end loop;

      Model.Set_Cell
        (Row, Last_Trade_Column,
         Harriet.Calendar.Image (Date));
      Model.Set_Cell
        (Row, Last_Price_Column,
         Harriet.Money.Show (Price));
      Model.Set_Cell
        (Row, Last_Quantity_Column,
         Harriet.Quantities.Show (Quantity));

   end Update_Commodity;

end Harriet.UI.Models.Market;
