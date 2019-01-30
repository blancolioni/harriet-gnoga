with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Calendar;
with Harriet.Logging;

with Harriet.Db.Ask_Offer;
with Harriet.Db.Bid_Offer;
with Harriet.Db.Market_Offer;
with Harriet.Db.Transaction;

package body Harriet.Markets is

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
      Message   : String);

   procedure Log_Market_Bid
     (Market    : Harriet.Db.Market_Reference;
      Agent     : Harriet.Db.Agent_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price     : Harriet.Money.Price_Type;
      Message   : String);

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
   begin

      Log_Market_Ask (Market, Agent, Commodity, Quantity, Price,
                      "looking for matching bids");

      for Bid of Harriet.Db.Bid_Offer.Select_Bounded_By_Market_Priority
        (Market, Commodity, 0.0, Market, Commodity, Real'Last)
      loop

         Log_Market_Bid
           (Market, Bid.Agent, Commodity, Bid.Quantity, Bid.Price,
            "checking");

         exit when Bid.Price < Price;

         declare
            This_Quantity : constant Quantity_Type :=
                              Min (Bid.Quantity, Remaining);
         begin
            Log_Market
              (Market, Agent, Commodity,
               "sell " & Show (This_Quantity)
               & " for " & Show (Total (Price, This_Quantity)));

            Harriet.Db.Transaction.Create
              (Time_Stamp => Harriet.Calendar.Clock,
               Market     => Market,
               Commodity  => Commodity,
               Buyer      => Bid.Agent,
               Seller     => Agent,
               Price      => Price,
               Quantity   => This_Quantity);
            Remaining := Remaining - Quantity;
         end;
         exit when Remaining = Zero;
      end loop;

      if Remaining > Zero then
         Harriet.Db.Ask_Offer.Create
           (Market    => Market,
            Commodity => Commodity,
            Agent     => Agent,
            Has_Stock => Has_Stock,
            Account   => Account,
            Price     => Price,
            Priority  => Harriet.Money.To_Real (Price),
            Quantity  => Remaining);
      end if;
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
      Ask (Market, Agent.Reference, Agent.Account, Agent.Reference,
           Commodity, Quantity, Price);
   end Ask;

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
   begin

      Log_Market_Bid (Market, Agent, Commodity, Quantity, Price,
                      "looking for matching asks");

      for Ask of Harriet.Db.Ask_Offer.Select_Bounded_By_Market_Priority
        (Market, Commodity, 0.0, Market, Commodity, Real'Last)
      loop

         Log_Market_Ask
           (Market, Ask.Agent, Commodity, Ask.Quantity, Ask.Price,
            "checking");

         exit when Ask.Price < Price;

         declare
            This_Quantity : constant Quantity_Type :=
                              Min (Ask.Quantity, Remaining);
         begin
            Log_Market
              (Market, Agent, Commodity,
               "buy " & Show (This_Quantity)
               & " for " & Show (Total (Price, This_Quantity)));

            Harriet.Db.Transaction.Create
              (Time_Stamp => Harriet.Calendar.Clock,
               Market     => Market,
               Commodity  => Commodity,
               Buyer      => Agent,
               Seller     => Ask.Agent,
               Price      => Price,
               Quantity   => This_Quantity);
            Remaining := Remaining - Quantity;
         end;
         exit when Remaining = Zero;
      end loop;

      if Remaining > Zero then
         Harriet.Db.Bid_Offer.Create
           (Market    => Market,
            Commodity => Commodity,
            Agent     => Agent,
            Has_Stock => Has_Stock,
            Account   => Account,
            Price     => Price,
            Priority  => 1.0 / Harriet.Money.To_Real (Price),
            Quantity  => Quantity);
      end if;

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
      Bid (Market, Agent.Reference, Agent.Account, Agent.Reference,
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

   ---------------
   -- First_Ask --
   ---------------

   function First_Ask
     (Market    : Harriet.Db.Market_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Db.Ask_Offer_Reference
   is
   begin
      for Ask of Harriet.Db.Ask_Offer.Select_Bounded_By_Market_Priority
        (Market, Commodity, 0.0, Market, Commodity, Real'Last)
      loop
         return Ask.Reference;
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
      for Bid of Harriet.Db.Bid_Offer.Select_Bounded_By_Market_Priority
        (Market, Commodity, 0.0, Market, Commodity, Real'Last)
      loop
         return Bid.Reference;
      end loop;
      return Harriet.Db.Null_Bid_Offer_Reference;
   end First_Bid;

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

   ------------------
   -- Reset_Offers --
   ------------------

   procedure Reset_Offers
     (Market : Harriet.Db.Market_Reference;
      Agent  : Harriet.Db.Agent_Reference)
   is
      package Offer_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Harriet.Db.Market_Offer_Reference,
           Harriet.Db."=");
      Offers : Offer_Lists.List;
   begin
      for Offer of
        Harriet.Db.Market_Offer.Select_By_Agent_Offer
          (Market, Agent)
      loop
         Offers.Append (Offer.Reference);
      end loop;

      for Reference of Offers loop
         declare
            Offer : Harriet.Db.Market_Offer.Market_Offer_Type :=
                      Harriet.Db.Market_Offer.Get (Reference);
         begin
            Offer.Delete;
         end;
      end loop;

   end Reset_Offers;

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
         for Ask of Harriet.Db.Ask_Offer.Select_Bounded_By_Market_Priority
           (Market, Commodity, 0.0, Market, Commodity, Real'Last)
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
