with Harriet.Agents;
with Harriet.Markets;
with Harriet.Worlds;

package body Harriet.Managers.Agents is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager)
   is
      M : Root_Agent_Manager'Class renames
            Root_Agent_Manager'Class (Manager.all);
   begin
      M.Create_Market_Offers;
      M.Execute_Agent_Tasks;
      Manager.Set_Next_Update_Delay
        (M.Next_Sleep_Duration);
   end Activate;

   ----------
   -- Cash --
   ----------

   function Cash
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Agents.Cash (Manager.Account);
   end Cash;

   ------------------------------
   -- Current_Market_Ask_Price --
   ------------------------------

   function Current_Market_Ask_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Current_Ask_Price
        (Manager.Market, Commodity);
   end Current_Market_Ask_Price;

   ------------------------------
   -- Current_Market_Bid_Price --
   ------------------------------

   function Current_Market_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Markets.Current_Bid_Price
        (Manager.Market, Commodity);
   end Current_Market_Bid_Price;

   -------------------
   -- Current_Stock --
   -------------------

   procedure Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Stock   : out Harriet.Commodities.Stock_Type)
   is
   begin
      Stock.Load (Manager.Has_Stock);
   end Current_Stock;

   ------------------------------
   -- Initialize_Agent_Manager --
   ------------------------------

   procedure Initialize_Agent_Manager
     (Manager : in out Root_Agent_Manager'Class;
      Agent   : Harriet.Db.Agent.Agent_Type;
      World   : Harriet.Db.World_Reference)
   is
   begin
      Manager.Agent := Agent.Reference;
      Manager.Has_Stock := Agent.Reference;
      Manager.Market := Harriet.Worlds.Market (World);
      Manager.Account := Agent.Account;
      Manager.World := World;
   end Initialize_Agent_Manager;

   -------------------------
   -- Next_Sleep_Duration --
   -------------------------

   function Next_Sleep_Duration
     (Manager : Root_Agent_Manager)
      return Duration
   is
      pragma Unreferenced (Manager);
   begin
      return Harriet.Calendar.Days (1.0);
   end Next_Sleep_Duration;

   ---------------
   -- Place_Ask --
   ---------------

   procedure Place_Ask
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Harriet.Markets.Ask
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Account   => Manager.Account,
         Has_Stock => Manager.Has_Stock,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);
   end Place_Ask;

   ---------------
   -- Place_Bid --
   ---------------

   procedure Place_Bid
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Harriet.Markets.Bid
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Account   => Manager.Account,
         Has_Stock => Manager.Has_Stock,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);
   end Place_Bid;

   --------------
   -- Try_Bids --
   --------------

   procedure Try_Bids
     (Manager   : Root_Agent_Manager'Class;
      Stock     : Harriet.Commodities.Stock_Type;
      Available : out Harriet.Commodities.Stock_Type)
   is
   begin
      Harriet.Markets.Try_Bid
        (Market    => Manager.Market,
         Wanted    => Stock,
         Available => Available);
   end Try_Bids;

   -----------
   -- World --
   -----------

   function World
     (Manager : Root_Agent_Manager'Class)
      return Harriet.Db.World_Reference
   is
   begin
      return Manager.World;
   end World;

end Harriet.Managers.Agents;
