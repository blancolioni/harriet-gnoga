with Harriet.Commodities;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Agent;

package Harriet.Managers.Agents is

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager);

   procedure Create_Market_Offers
     (Manager : in out Root_Agent_Manager)
   is abstract;

   procedure Get_Required_Stock
     (Manager : Root_Agent_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is abstract;

   procedure Execute_Agent_Tasks
     (Manager : in out Root_Agent_Manager)
   is abstract;

   function Next_Sleep_Duration
     (Manager : Root_Agent_Manager)
     return Duration;

   function Current_Market_Ask_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Current_Market_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function World
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Db.World_Reference;

   procedure Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Stock   : out Harriet.Commodities.Stock_Type);

   procedure Try_Bids
     (Manager   : Root_Agent_Manager'Class;
      Stock     : Harriet.Commodities.Stock_Type;
      Available : out Harriet.Commodities.Stock_Type);

   procedure Place_Bid
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   procedure Place_Ask
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   function Cash
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Money.Money_Type;

   procedure Initialize_Agent_Manager
     (Manager : in out Root_Agent_Manager'Class;
      Agent   : Harriet.Db.Agent.Agent_Type;
      World   : Harriet.Db.World_Reference);

private

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with
      record
         Agent        : Harriet.Db.Agent_Reference;
         Has_Stock    : Harriet.Db.Has_Stock_Reference;
         Market       : Harriet.Db.Market_Reference;
         Account      : Harriet.Db.Account_Reference;
         World        : Harriet.Db.World_Reference;
      end record;

end Harriet.Managers.Agents;
