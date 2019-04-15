with Harriet.Commodities;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Agent;

package Harriet.Markets is

   function Current_Ask_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Current_Bid_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Daily_Demand
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Daily_Supply
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Demand
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Days      : Non_Negative_Real)
      return Harriet.Quantities.Quantity_Type;

   function Historical_Supply
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Days      : Non_Negative_Real)
      return Harriet.Quantities.Quantity_Type;

   function Current_Demand
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Current_Supply
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   procedure Reset_Offers
     (Market : Harriet.Db.Market_Reference;
      Agent  : Harriet.Db.Agent_Reference);

   procedure Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent.Agent_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Priority  : Non_Negative_Real);

   procedure Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent.Agent_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Priority  : Non_Negative_Real);

   procedure Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Priority  : Non_Negative_Real);

   procedure Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Priority  : Non_Negative_Real);

   procedure Try_Bid
     (Market    : Harriet.Db.Market_Reference;
      Wanted    : Harriet.Commodities.Stock_Type;
      Available : out Harriet.Commodities.Stock_Type);

   procedure Initialize_Markets;

   procedure Update_Market
     (Market : Harriet.Db.Market_Reference);

   type Market_Handler_Id is private;

   type Market_Data is interface;

   type Market_Offer_Handler is access
     procedure (Data      : Market_Data'Class;
                Offer     : Harriet.Db.Offer_Type;
                Commodity : Harriet.Db.Commodity_Reference;
                Quantity  : Harriet.Quantities.Quantity_Type;
                Price     : Harriet.Money.Price_Type);

   type Market_Transaction_Handler is access
     procedure (Data      : Market_Data'Class;
                Commodity : Harriet.Db.Commodity_Reference;
                Quantity  : Harriet.Quantities.Quantity_Type;
                Price     : Harriet.Money.Price_Type);

   function Add_Market_Watcher
     (Market         : Harriet.Db.Market_Reference;
      Data           : Market_Data'Class;
      On_Offer       : Market_Offer_Handler;
      On_Transaction : Market_Transaction_Handler)
      return Market_Handler_Id;

   procedure Remove_Market_Watcher
     (Market : Harriet.Db.Market_Reference;
      Id     : Market_Handler_Id);

private

   type Market_Handler_Id is new Positive;

end Harriet.Markets;
