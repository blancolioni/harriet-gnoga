with Harriet.Commodities;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Agent;

package Harriet.Managers.Agents is

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager);

   function Managed_Object_Id
     (Manager : Root_Agent_Manager)
      return String
      is abstract;

   procedure Create_Market_Offers
     (Manager : in out Root_Agent_Manager);

   procedure Get_Required_Stock
     (Manager  : Root_Agent_Manager;
      Stock    : in out Harriet.Commodities.Stock_Type)
   is abstract;

   procedure Get_Desired_Stock
     (Manager  : Root_Agent_Manager;
      Stock    : in out Harriet.Commodities.Stock_Type);

   procedure Execute_Agent_Tasks
     (Manager : in out Root_Agent_Manager)
   is abstract;

   function Calculate_Capacity
     (Manager : Root_Agent_Manager;
      Stock   : Harriet.Commodities.Stock_Type)
      return Unit_Real;

   function Next_Sleep_Duration
     (Manager : Root_Agent_Manager)
     return Duration;

   function Current_Market
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Db.Market_Reference;

   function Current_Market_Ask_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Current_Market_Ask_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Current_Market_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   function Current_Market_Bid_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Current_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   procedure Set_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Price     : Harriet.Money.Price_Type);

   function World
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Db.World_Reference;

   procedure Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Stock   : out Harriet.Commodities.Stock_Type);

   function Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   procedure Add_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Value     : Harriet.Money.Money_Type);

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   procedure Scan_Current_Stock
     (Manager   : Root_Agent_Manager'Class;
      Process   : not null access
        procedure (Item     : Harriet.Db.Commodity_Reference;
                   Quantity : Harriet.Quantities.Quantity_Type;
                   Value    : Harriet.Money.Money_Type));

   procedure Scan_Historical_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Days      : Non_Negative_Real;
      Process   : not null access
        procedure (Date : Harriet.Calendar.Time;
                   Quantity : Harriet.Quantities.Quantity_Type));

   procedure Try_Bids
     (Manager   : Root_Agent_Manager'Class;
      Stock     : Harriet.Commodities.Stock_Type;
      Available : out Harriet.Commodities.Stock_Type);

   procedure Place_Bid
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   procedure Place_Ask
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   function Capacity
     (Manager : Root_Agent_Manager'Class)
      return Unit_Real;

   function Cash
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Money.Money_Type;

   procedure Pay
     (Manager : Root_Agent_Manager'Class;
      Amount  : Harriet.Money.Money_Type);

   procedure Earn
     (Manager : Root_Agent_Manager'Class;
      Amount  : Harriet.Money.Money_Type);

   procedure Log
     (Manager : Root_Agent_Manager'Class;
      Message : String);

   procedure Log_Market_State
     (Manager : Root_Agent_Manager'Class);

   procedure Initialize_Agent_Manager
     (Manager  : in out Root_Agent_Manager'Class;
      Agent    : Harriet.Db.Agent.Agent_Type;
      World    : Harriet.Db.World_Reference;
      Priority : Non_Negative_Real);

private

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with
      record
         Agent        : Harriet.Db.Agent_Reference;
         Has_Stock    : Harriet.Db.Has_Stock_Reference;
         Market       : Harriet.Db.Market_Reference;
         Account      : Harriet.Db.Account_Reference;
         World        : Harriet.Db.World_Reference;
         Capacity     : Unit_Real := 1.0;
         Priority     : Non_Negative_Real;
      end record;

   function Capacity
     (Manager : Root_Agent_Manager'Class)
      return Unit_Real
   is (Manager.Capacity);

   function Current_Market
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Db.Market_Reference
   is (Manager.Market);

end Harriet.Managers.Agents;
