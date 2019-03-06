with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;

with Harriet.Calendar;
with Harriet.Logging;

with Harriet.Agents;
with Harriet.Employment;
with Harriet.Stock;

with Harriet.Db.Ask_Offer;
with Harriet.Db.Bid_Offer;
with Harriet.Db.Market_Offer;
with Harriet.Db.Transaction;

with Harriet.Db.Historical_Ask;
with Harriet.Db.Historical_Bid;

with Harriet.Db.Market_Maps;

package body Harriet.Markets is

   package Ask_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.Ask_Offer_Reference, Harriet.Db."=");

   package Bid_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.Bid_Offer_Reference, Harriet.Db."=");

   package Market_Data_Holder is
     new Ada.Containers.Indefinite_Holders (Market_Data'Class);

   type Market_Watcher is
      record
         Id             : Market_Handler_Id;
         Data           : Market_Data_Holder.Holder;
         On_Offer       : Market_Offer_Handler;
         On_Transaction : Market_Transaction_Handler;
      end record;

   package Market_Watcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Market_Watcher);

   package Market_Dispatcher_Maps is
     new Harriet.Db.Market_Maps
       (Element_Type => Market_Watcher_Lists.List,
        "="          => Market_Watcher_Lists."=");

   Market_Dispatcher : Market_Dispatcher_Maps.Map;
   Next_Id : Market_Handler_Id := 1;

   procedure Notify_New_Offer
     (Reference : Harriet.Db.Market_Offer_Reference);

   procedure Notify_New_Transaction
     (Reference : Harriet.Db.Transaction_Reference);

   function First_Ask
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Db.Ask_Offer_Reference;

   function First_Bid
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
     return Harriet.Db.Bid_Offer_Reference;

   procedure Log_Market
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Message   : String);

   procedure Log_Market_Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type;
      Message   : String)
     with Unreferenced;

   procedure Log_Market_Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type;
      Message   : String)
     with Unreferenced;

   procedure Execute_Ask_Offer
     (Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   procedure Execute_Bid_Offer
     (Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type);

   ------------------------
   -- Add_Market_Watcher --
   ------------------------

   function Add_Market_Watcher
     (Market         : Harriet.Db.Market_Reference;
      Data           : Market_Data'Class;
      On_Offer       : Market_Offer_Handler;
      On_Transaction : Market_Transaction_Handler)
      return Market_Handler_Id
   is
      List : Market_Watcher_Lists.List;
      Is_New : constant Boolean := not Market_Dispatcher.Contains (Market);
   begin
      if not Is_New then
         List := Market_Dispatcher.Element (Market);
      end if;
      List.Append
        (Market_Watcher'
           (Id             => Next_Id,
            Data           => Market_Data_Holder.To_Holder (Data),
            On_Offer       => On_Offer,
            On_Transaction => On_Transaction));
      if Is_New then
         Market_Dispatcher.Insert (Market, List);
      else
         Market_Dispatcher.Replace_Element (Market, List);
      end if;
      Next_Id := Next_Id + 1;
      return List.Last_Element.Id;
   end Add_Market_Watcher;

   ---------
   -- Ask --
   ---------

   procedure Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
      use Harriet.Money;
      use Harriet.Quantities;
      Remaining : Quantity_Type := Quantity;
      Completed : Bid_Lists.List;

   begin

      Harriet.Db.Historical_Ask.Create
        (Time_Stamp => Harriet.Calendar.Clock,
         Market     => Market,
         Commodity  => Commodity,
         Agent      => Agent,
         Offer      => Harriet.Db.Ask,
         Quantity   => Quantity,
         Price      => Price);

      for Bid of
        Harriet.Db.Bid_Offer.Select_Market_Priority_Bounded_By_Priority
          (Market, Commodity, 0.0, Real'Last)
      loop

         if Bid.Quantity = Zero then
            Completed.Append (Bid.Get_Bid_Offer_Reference);
         end if;

         exit when Bid.Price < Price;

         declare
            This_Quantity : constant Quantity_Type :=
                              Min (Bid.Quantity, Remaining);
         begin
            if This_Quantity > Zero then
               if Harriet.Commodities.Is_Pop_Group (Commodity) then
                  Log_Market
                    (Market, Bid.Agent, Commodity,
                     "employ " & Show (This_Quantity)
                     & " for " & Show (Total (Price, This_Quantity)));
                  Harriet.Employment.Create_Employment_Contract
                    (Employer => Bid.Agent,
                     Employee => Agent,
                     Quantity => This_Quantity,
                     Salary   => Price);
               else
                  Log_Market
                    (Market, Agent, Commodity,
                     "sell " & Show (This_Quantity)
                     & " of " & Show (Bid.Quantity)
                     & " to Agent" & Harriet.Db.To_String (Bid.Agent)
                     & " for "
                     & Show (Price)
                     & " ea; total "
                     & Show (Total (Price, This_Quantity)));
               end if;

               Execute_Ask_Offer
                 (Account   => Account,
                  Has_Stock => Has_Stock,
                  Commodity => Commodity,
                  Quantity  => This_Quantity,
                  Price     => Price);

               Execute_Bid_Offer
                 (Account   => Bid.Account,
                  Has_Stock => Bid.Has_Stock,
                  Commodity => Commodity,
                  Quantity  => This_Quantity,
                  Price     => Price);

               Bid.Set_Quantity (Bid.Quantity - This_Quantity);
               if Bid.Quantity = Zero then
                  Completed.Append (Bid.Get_Bid_Offer_Reference);
               end if;

               Harriet.Db.Transaction.Create
                 (Time_Stamp => Harriet.Calendar.Clock,
                  Market     => Market,
                  Commodity  => Commodity,
                  Buyer      => Bid.Agent,
                  Seller     => Agent,
                  Price      => Price,
                  Quantity   => This_Quantity);
               Remaining := Remaining - This_Quantity;
            end if;
         end;
         exit when Remaining = Zero;
      end loop;

      declare
         Offer : constant Harriet.Db.Ask_Offer.Ask_Offer_Type :=
                   Harriet.Db.Ask_Offer.Get_By_Market_Offer
                     (Market    => Market,
                      Agent     => Agent,
                      Commodity => Commodity,
                      Offer     => Harriet.Db.Ask);
      begin
         if Offer.Has_Element then
            Offer.Set_Price (Price);
            Offer.Set_Priority (Harriet.Money.To_Real (Price));
            Offer.Set_Quantity (Remaining);
         elsif Remaining > Zero then
            Harriet.Db.Ask_Offer.Create
              (Market    => Market,
               Commodity => Commodity,
               Offer     => Harriet.Db.Ask,
               Agent     => Agent,
               Has_Stock => Has_Stock,
               Account   => Account,
               Price     => Price,
               Priority  => Harriet.Money.To_Real (Price),
               Quantity  => Remaining);
         end if;
      end;

   end Ask;

   ---------
   -- Ask --
   ---------

   procedure Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent.Agent_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Ask (Market, Agent.Get_Agent_Reference, Agent.Account,
           Agent.Get_Has_Stock_Reference,
           Commodity, Quantity, Price);
   end Ask;

   ----------------------------
   -- Available_At_Bid_Price --
   ----------------------------

   function Available_At_Bid_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Price     : Harriet.Money.Price_Type)
      return Harriet.Quantities.Quantity_Type
   is
      use Harriet.Money, Harriet.Quantities;
      Available  : Quantity_Type := Zero;
   begin
      for Ask of
        Harriet.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
          (Market, Commodity, 0.0, Real'Last)
      loop
         exit when Ask.Price > Price;
         Available := Available + Ask.Quantity;
      end loop;
      return Available;
   end Available_At_Bid_Price;

   ---------
   -- Bid --
   ---------

   procedure Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
      use Harriet.Money;
      use Harriet.Quantities;
      Remaining : Quantity_Type := Quantity;
      Completed : Ask_Lists.List;
   begin

      Harriet.Db.Historical_Bid.Create
        (Time_Stamp => Harriet.Calendar.Clock,
         Market     => Market,
         Commodity  => Commodity,
         Agent      => Agent,
         Offer      => Harriet.Db.Bid,
         Quantity   => Quantity,
         Price      => Price);

      for Ask of
        Harriet.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
          (Market, Commodity, 0.0, Real'Last)
      loop

         if Ask.Quantity = Zero then
            Completed.Append (Ask.Get_Ask_Offer_Reference);
         end if;

         exit when Ask.Price > Price;

         declare
            This_Quantity : constant Quantity_Type :=
                              Min (Ask.Quantity, Remaining);
         begin

            if This_Quantity > Zero then
               if Harriet.Commodities.Is_Pop_Group (Commodity) then
                  Log_Market
                    (Market, Agent, Commodity,
                     "employ " & Show (This_Quantity)
                     & " for " & Show (Price)
                     & " ea; total "
                     & Show (Total (Price, This_Quantity)));
                  Harriet.Employment.Create_Employment_Contract
                    (Employer => Agent,
                     Employee => Ask.Agent,
                     Quantity => This_Quantity,
                     Salary   => Price);
               else
                  Log_Market
                    (Market, Agent, Commodity,
                     "buy " & Show (This_Quantity)
                     & " of " & Show (Ask.Quantity)
                     & " from Agent" & Harriet.Db.To_String (Ask.Agent)
                     & " for "
                     & Show (Price)
                     & " ea; total "
                     & Show (Total (Price, This_Quantity)));

               end if;

               Execute_Bid_Offer
                 (Account   => Account,
                  Has_Stock => Has_Stock,
                  Commodity => Commodity,
                  Quantity  => This_Quantity,
                  Price     => Price);

               Execute_Ask_Offer
                 (Account   => Ask.Account,
                  Has_Stock => Ask.Has_Stock,
                  Commodity => Commodity,
                  Quantity  => This_Quantity,
                  Price     => Price);

               Ask.Set_Quantity (Ask.Quantity - This_Quantity);
               if Ask.Quantity = Zero then
                  Completed.Append (Ask.Get_Ask_Offer_Reference);
               end if;

               Harriet.Db.Transaction.Create
                 (Time_Stamp => Harriet.Calendar.Clock,
                  Market     => Market,
                  Commodity  => Commodity,
                  Buyer      => Agent,
                  Seller     => Ask.Agent,
                  Price      => Price,
                  Quantity   => This_Quantity);

               Remaining := Remaining - This_Quantity;
            end if;
         end;
         exit when Remaining = Zero;
      end loop;

      declare
         Offer : constant Harriet.Db.Bid_Offer.Bid_Offer_Type :=
                   Harriet.Db.Bid_Offer.Get_By_Market_Offer
                     (Market    => Market,
                      Agent     => Agent,
                      Commodity => Commodity,
                      Offer     => Harriet.Db.Bid);
      begin
         if Offer.Has_Element then
            Offer.Set_Price (Price);
            Offer.Set_Priority (1.0 / Harriet.Money.To_Real (Price));
            Offer.Set_Quantity (Remaining);
         elsif Remaining > Zero then
            Harriet.Db.Bid_Offer.Create
              (Market    => Market,
               Commodity => Commodity,
               Offer     => Harriet.Db.Bid,
               Agent     => Agent,
               Has_Stock => Has_Stock,
               Account   => Account,
               Price     => Price,
               Priority  => 1.0 / Harriet.Money.To_Real (Price),
               Quantity  => Quantity);
         end if;
      end;

   end Bid;

   ---------
   -- Bid --
   ---------

   procedure Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent.Agent_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Bid (Market, Agent.Get_Agent_Reference, Agent.Account,
           Agent.Get_Has_Stock_Reference,
           Commodity, Quantity, Price);
   end Bid;

   -----------------------
   -- Current_Ask_Price --
   -----------------------

   function Current_Ask_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
      use Harriet.Db;
      Ask_Ref : constant Harriet.Db.Ask_Offer_Reference :=
                  First_Ask (Market, Commodity);
   begin
      if Ask_Ref = Null_Ask_Offer_Reference then
         return Harriet.Commodities.Initial_Price (Commodity);
      else
         return Harriet.Db.Ask_Offer.Get (Ask_Ref).Price;
      end if;
   end Current_Ask_Price;

   -----------------------
   -- Current_Bid_Price --
   -----------------------

   function Current_Bid_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
      use Harriet.Db;
      Bid_Ref : constant Harriet.Db.Bid_Offer_Reference :=
                  First_Bid (Market, Commodity);
   begin
      if Bid_Ref = Null_Bid_Offer_Reference then
         return Harriet.Commodities.Initial_Price (Commodity);
      else
         return Harriet.Db.Bid_Offer.Get (Bid_Ref).Price;
      end if;
   end Current_Bid_Price;

   ------------------
   -- Daily_Demand --
   ------------------

   function Daily_Demand
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      use type Harriet.Calendar.Time;
      use type Harriet.Quantities.Quantity_Type;
      Now : constant Harriet.Calendar.Time := Harriet.Calendar.Clock;
      Quantity : Harriet.Quantities.Quantity_Type :=
                   Harriet.Quantities.Zero;
   begin
      for Offer of
        Harriet.Db.Historical_Bid
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Market, Commodity, Now - Harriet.Calendar.Days (1.0), Now)
      loop
         Quantity := Quantity + Offer.Quantity;
      end loop;
      return Quantity;
   end Daily_Demand;

   ------------------
   -- Daily_Supply --
   ------------------

   function Daily_Supply
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      use type Harriet.Calendar.Time;
      use type Harriet.Quantities.Quantity_Type;
      Now : constant Harriet.Calendar.Time := Harriet.Calendar.Clock;
      Quantity : Harriet.Quantities.Quantity_Type :=
                   Harriet.Quantities.Zero;
   begin
      for Offer of
        Harriet.Db.Historical_Ask
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Market, Commodity, Now - Harriet.Calendar.Days (1.0), Now)
      loop
         Quantity := Quantity + Offer.Quantity;
      end loop;
      return Quantity;
   end Daily_Supply;

   -----------------------
   -- Execute_Ask_Offer --
   -----------------------

   procedure Execute_Ask_Offer
     (Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
   begin
      Harriet.Stock.Remove_Stock
        (From     => Has_Stock,
         Item     => Commodity,
         Quantity => Quantity);
      Harriet.Agents.Add_Cash
        (Account => Account,
         Cash    => Harriet.Money.Total (Price, Quantity));
   end Execute_Ask_Offer;

   -----------------------
   -- Execute_Bid_Offer --
   -----------------------

   procedure Execute_Bid_Offer
     (Account   : Harriet.Db.Account_Reference;
      Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type)
   is
      Cost : constant Harriet.Money.Money_Type :=
               Harriet.Money.Total (Price, Quantity);
   begin
      Harriet.Stock.Add_Stock
        (To       => Has_Stock,
         Item     => Commodity,
         Quantity => Quantity,
         Value    => Cost);

      Harriet.Agents.Remove_Cash
        (Account => Account,
         Cash    => Cost);

   end Execute_Bid_Offer;

   ---------------
   -- First_Ask --
   ---------------

   function First_Ask
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Db.Ask_Offer_Reference
   is
   begin
      for Ask of
        Harriet.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
          (Market, Commodity, 0.0, Real'Last)
      loop
         return Ask.Get_Ask_Offer_Reference;
      end loop;
      return Harriet.Db.Null_Ask_Offer_Reference;
   end First_Ask;

   ---------------
   -- First_Bid --
   ---------------

   function First_Bid
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Db.Bid_Offer_Reference
   is
   begin
      for Bid of
        Harriet.Db.Bid_Offer.Select_Market_Priority_Bounded_By_Priority
        (Market, Commodity, 0.0, Real'Last)
      loop
         return Bid.Get_Bid_Offer_Reference;
      end loop;
      return Harriet.Db.Null_Bid_Offer_Reference;
   end First_Bid;

   ------------------------
   -- Initialize_Markets --
   ------------------------

   procedure Initialize_Markets is
   begin
      Harriet.Db.Market_Offer.On_Market_Offer_Created
        (Notify_New_Offer'Access);
      Harriet.Db.Transaction.On_Transaction_Created
        (Notify_New_Transaction'Access);
   end Initialize_Markets;

   ----------------
   -- Log_Market --
   ----------------

   procedure Log_Market
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Message   : String)
   is
   begin
      Harriet.Logging.Log
        (Actor    => "Agent" & Harriet.Db.To_String (Agent),
         Location => "Market" & Harriet.Db.To_String (Market),
         Category => Harriet.Commodities.Local_Name (Commodity),
         Message  => Message);
   end Log_Market;

   --------------------
   -- Log_Market_Ask --
   --------------------

   procedure Log_Market_Ask
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type;
      Message   : String)
   is
      Total : constant Harriet.Money.Money_Type :=
                Harriet.Money.Total (Price, Quantity);
   begin
      Log_Market (Market, Agent, Commodity,
                  "ask "
                  & Harriet.Money.Show (Total)
                  & " for "
                  & Harriet.Quantities.Show (Quantity)
                  & " ("
                  & Harriet.Money.Show (Price)
                  & " ea)"
                  & ": "
                  & Message);
   end Log_Market_Ask;

   --------------------
   -- Log_Market_Bid --
   --------------------

   procedure Log_Market_Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type;
      Message   : String)
   is
      Total : constant Harriet.Money.Money_Type :=
                Harriet.Money.Total (Price, Quantity);
   begin
      Log_Market (Market, Agent, Commodity,
                  "bid "
                  & Harriet.Money.Show (Total)
                  & " for "
                  & Harriet.Quantities.Show (Quantity)
                  & " ("
                  & Harriet.Money.Show (Price)
                  & " ea)"
                  & ": "
                  & Message);
   end Log_Market_Bid;

   -----------------------
   -- Minimum_Bid_Price --
   -----------------------

   function Minimum_Bid_Price
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type)
      return Harriet.Money.Price_Type
   is
      use Harriet.Money, Harriet.Quantities;
      Available  : Quantity_Type := Zero;
      Last_Price : Price_Type := Zero;
   begin
      for Ask of
        Harriet.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
          (Market, Commodity, 0.0, Real'Last)
      loop
         Available := Available + Ask.Quantity;
         if Available >= Quantity then
            return Ask.Price;
         end if;
         Last_Price := Ask.Price;
      end loop;
      if Last_Price > Zero then
         return Adjust_Price (Last_Price, 1.1);
      else
         return Current_Ask_Price (Market, Commodity);
      end if;
   end Minimum_Bid_Price;

   ----------------------
   -- Notify_New_Offer --
   ----------------------

   procedure Notify_New_Offer
     (Reference : Harriet.Db.Market_Offer_Reference)
   is
      Offer  : constant Harriet.Db.Market_Offer.Market_Offer_Type :=
                 Harriet.Db.Market_Offer.Get (Reference);
      Market : constant Harriet.Db.Market_Reference := Offer.Market;
   begin
      if Market_Dispatcher.Contains (Market) then
         for Item of Market_Dispatcher.Element (Market) loop
            Item.On_Offer
              (Item.Data.Element, Offer.Offer,
               Offer.Commodity, Offer.Quantity, Offer.Price);
         end loop;
      end if;
   end Notify_New_Offer;

   ----------------------------
   -- Notify_New_Transaction --
   ----------------------------

   procedure Notify_New_Transaction
     (Reference : Harriet.Db.Transaction_Reference)
   is
      Transaction : constant Harriet.Db.Transaction.Transaction_Type :=
                      Harriet.Db.Transaction.Get (Reference);
      Market      : constant Harriet.Db.Market_Reference :=
                      Transaction.Market;
   begin
      if Market_Dispatcher.Contains (Market) then
         for Item of Market_Dispatcher.Element (Market) loop
            Item.On_Transaction
              (Item.Data.Element,
               Transaction.Commodity, Transaction.Quantity, Transaction.Price);
         end loop;
      end if;
   end Notify_New_Transaction;

   ---------------------------
   -- Remove_Market_Watcher --
   ---------------------------

   procedure Remove_Market_Watcher
     (Market : Harriet.Db.Market_Reference;
      Id     : Market_Handler_Id)
   is null;

   ------------------
   -- Reset_Offers --
   ------------------

   procedure Reset_Offers
     (Market : Harriet.Db.Market_Reference;
      Agent  : Harriet.Db.Agent_Reference)
   is null;

--        package Offer_Lists is
--          new Ada.Containers.Doubly_Linked_Lists
--            (Harriet.Db.Market_Offer_Reference,
--             Harriet.Db."=");
--        Offers : Offer_Lists.List;
--     begin
--        for Offer of
--          Harriet.Db.Market_Offer.Select_By_Agent_Offer
--            (Market, Agent)
--        loop
--           Offers.Append (Offer.Get_Market_Offer_Reference);
--        end loop;
--
--        for Reference of Offers loop
--           declare
--              Offer : Harriet.Db.Market_Offer.Market_Offer_Type :=
--                        Harriet.Db.Market_Offer.Get (Reference);
--           begin
--              Log_Market (Market, Agent, Offer.Commodity,
--                          "delete "
--                          & Harriet.Db.Offer_Type'Image (Offer.Offer)
--                          & ": "
--                          & Harriet.Quantities.Show (Offer.Quantity)
--                          & " @ "
--                          & Harriet.Money.Show (Offer.Price)
--                          & " ea");
--              Offer.Delete;
--           end;
--        end loop;
--
--     end Reset_Offers;

   -------------
   -- Try_Bid --
   -------------

   procedure Try_Bid
     (Market    : Harriet.Db.Market_Reference;
      Wanted    : Harriet.Commodities.Stock_Type;
      Available : out Harriet.Commodities.Stock_Type)
   is
      procedure Check_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type);

      ---------------
      -- Check_Bid --
      ---------------

      procedure Check_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type)
      is
         pragma Unreferenced (Value);
         use Harriet.Money, Harriet.Quantities;
         This_Quantity : Quantity_Type := Zero;
         This_Cost     : Money_Type    := Zero;
      begin
         for Ask of
           Harriet.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
           (Market, Commodity, 0.0, Real'Last)
         loop
            declare
               Bid_Quantity : constant Quantity_Type :=
                                Min (Ask.Quantity, Quantity - This_Quantity);
            begin
               This_Cost := This_Cost + Total (Ask.Price, Bid_Quantity);
               This_Quantity := This_Quantity + Bid_Quantity;
               exit when This_Quantity >= Quantity;
            end;
         end loop;

         Available.Set_Quantity (Commodity, This_Quantity, This_Cost);
      end Check_Bid;

   begin
      Wanted.Iterate (Check_Bid'Access);
   end Try_Bid;

end Harriet.Markets;
