with Ada.Text_IO;

with Harriet.Commodities;
with Harriet.Money;

with Harriet.Db.Ask_Offer;
with Harriet.Db.Bid_Offer;
with Harriet.Db.Commodity;

package body Harriet.UI.Models.Market is

   type Market_Signal_Data is
     new Harriet.Signals.Signal_Data_Interface with
      record
         Model : Market_Model;
      end record;

   procedure Handle_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class);

   procedure Create_Table
     (Model : in out Root_Market_Model'Class);

   ------------
   -- Create --
   ------------

   function Create
     (Session : Harriet.Sessions.Harriet_Session;
      Market  : Harriet.Db.Market_Reference)
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
            Data : constant Market_Signal_Data :=
                     (Model => Model);
         begin
            Model.Clock_Handler_Id :=
              Session.Add_Handler
                (Signal  => Harriet.Sessions.Signal_Clock_Tick,
                 Handler => Handle_Clock_Tick'Access,
                 Data    => Data);
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
   end Create_Table;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   procedure Handle_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
      Model : constant Market_Model :=
                Market_Signal_Data (Data).Model;
   begin
      Ada.Text_IO.Put_Line ("updating market");
      Model.Create_Table;
      Model.Notify_Changed;
   end Handle_Clock_Tick;

end Harriet.UI.Models.Market;
