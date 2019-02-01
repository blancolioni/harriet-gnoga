with Harriet.Logging;
--  with Harriet.Real_Images;

with Harriet.Agents;
with Harriet.Markets;
with Harriet.Stock;
with Harriet.Worlds;

with Harriet.Db.Historical_Stock;

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
      Harriet.Markets.Reset_Offers (Manager.Market, Manager.Agent);
      M.Create_Market_Offers;
      M.Execute_Agent_Tasks;
      Manager.Set_Next_Update_Delay
        (M.Next_Sleep_Duration);
   end Activate;

   ------------------------
   -- Calculate_Capacity --
   ------------------------

   function Calculate_Capacity
     (Manager : Root_Agent_Manager;
      Stock   : Harriet.Commodities.Stock_Type)
      return Unit_Real
   is
      Capacity : Unit_Real := 1.0;

      procedure Limit_Capacity
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type);

      --------------------
      -- Limit_Capacity --
      --------------------

      procedure Limit_Capacity
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type)
      is
         pragma Unreferenced (Value);
         use Harriet.Quantities;
         Required  : constant Quantity_Type := Quantity;
         Have      : constant Quantity_Type :=
                       Stock.Get_Quantity (Commodity);
         Limit     : constant Non_Negative_Real :=
                       To_Real (Have) / To_Real (Required);
      begin
         if Limit < Capacity then
            Capacity := Limit;
         end if;
      end Limit_Capacity;

      Required_Stock : Harriet.Commodities.Stock_Type;

   begin
      Root_Agent_Manager'Class (Manager).Get_Required_Stock (Required_Stock);

      Required_Stock.Iterate (Limit_Capacity'Access);

      return Capacity;

   end Calculate_Capacity;

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

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   procedure Create_Market_Offers
     (Manager : in out Root_Agent_Manager)
   is
      use Harriet.Commodities;
      use Harriet.Money;
      use Harriet.Quantities;
      M         : Root_Agent_Manager'Class renames
                    Root_Agent_Manager'Class (Manager);
      Have      : Harriet.Commodities.Stock_Type;
      Required  : Harriet.Commodities.Stock_Type;
      Missing   : Harriet.Commodities.Stock_Type;
--        Available : Harriet.Commodities.Stock_Type;
--        Cost      : Harriet.Money.Money_Type;
--        Cash      : constant Harriet.Money.Money_Type := Manager.Cash;

      procedure Place_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantity_Type;
         Value     : Money_Type);

      ---------------
      -- Place_Bid --
      ---------------

      procedure Place_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantity_Type;
         Value     : Money_Type)
      is
         pragma Unreferenced (Value);
         Price : constant Harriet.Money.Price_Type :=
                   Manager.Current_Market_Ask_Price (Commodity);
      begin
         Manager.Place_Bid (Commodity, Quantity, Price);
      end Place_Bid;

   begin
      Harriet.Logging.Log
        (Actor    => "Agent" & Harriet.Db.To_String (Manager.Agent),
         Location => Harriet.Worlds.Name (Manager.World),
         Category => "manager",
         Message  => "create market offers");
      Manager.Current_Stock (Have);
      Manager.Capacity := 1.0;
      M.Get_Required_Stock (Required);
      Missing := Harriet.Commodities.Missing (Have, Required);

      if Missing.Total_Quantity > Zero then
--           Manager.Try_Bids (Missing, Available);
--           Cost := Available.Total_Value;
--
--           if Cost > Cash then
--              declare
--                 Base : constant Unit_Real :=
--                          M.Calculate_Capacity (Have);
--              begin
--                 Manager.Capacity :=
--                   Base +
--                     (Manager.Capacity - Base)
--                   * To_Real (Cash) / To_Real (Cost);
--              end;
--           end if;
--
--           Harriet.Logging.Log
--             (Actor    => "Agent" & Harriet.Db.To_String (Manager.Agent),
--              Location => Harriet.Worlds.Name (Manager.World),
--              Category => "manager",
--              Message  =>
--                "available commodities "
--              & Harriet.Quantities.Show (Available.Total_Quantity)
--              & "; cost of missing commodities: "
--              & Harriet.Money.Show (Cost)
--              & "; cash "
--              & Harriet.Money.Show (Cash)
--              & "; estimated capacity: "
--              & Harriet.Real_Images.Approximate_Image (Manager.Capacity));
--
--           Missing := Available.Missing (Required);

         Harriet.Logging.Log
           (Actor    => "Agent" & Harriet.Db.To_String (Manager.Agent),
            Location => Harriet.Worlds.Name (Manager.World),
            Category => "manager",
            Message  =>
              "bidding on "
            & Harriet.Quantities.Show (Missing.Total_Quantity)
            & " items");

         Missing.Iterate (Place_Bid'Access);
      end if;
   end Create_Market_Offers;

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

   -------------------
   -- Current_Stock --
   -------------------

   function Current_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Stock.Get_Quantity (Manager.Has_Stock, Commodity);
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

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
   is
   begin
      Harriet.Stock.Remove_Stock
        (Manager.Has_Stock, Commodity, Quantity);
   end Remove_Stock;

   ---------------------------
   -- Scan_Historical_Stock --
   ---------------------------

   procedure Scan_Historical_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Harriet.Db.Commodity_Reference;
      Days      : Non_Negative_Real;
      Process   : not null access
        procedure (Date : Harriet.Calendar.Time;
                   Quantity : Harriet.Quantities.Quantity_Type))
   is
      use Harriet.Calendar;
      Now   : constant Time := Clock;
      Start : constant Time := Now - Harriet.Calendar.Days (Days);
   begin
      for Historical_Stock of
        Harriet.Db.Historical_Stock
          .Select_Historical_Stock_Bounded_By_Time_Stamp
            (Has_Stock         => Manager.Has_Stock,
             Commodity         => Commodity,
             Start_Time_Stamp  => To_Real (Start),
             Finish_Time_Stamp => To_Real (Now))
      loop
         Process (To_Time (Historical_Stock.Time_Stamp),
                  Historical_Stock.Quantity);
      end loop;
   end Scan_Historical_Stock;

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
