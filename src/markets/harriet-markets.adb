with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Db.Market_Offer;
with Harriet.Db.Ask_Offer;
with Harriet.Db.Bid_Offer;

package body Harriet.Markets is

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
      Harriet.Db.Ask_Offer.Create
        (Market    => Market,
         Commodity => Commodity,
         Agent     => Agent.Reference,
         Price     => Price,
         Priority  => 1.0 / Harriet.Money.To_Real (Price),
         Quantity  => Quantity);
   end Ask;

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
      Harriet.Db.Bid_Offer.Create
        (Market    => Market,
         Commodity => Commodity,
         Agent     => Agent.Reference,
         Price     => Price,
         Priority  => Harriet.Money.To_Real (Price),
         Quantity  => Quantity);
   end Bid;

   ------------------
   -- Reset_Offers --
   ------------------

   procedure Reset_Offers
     (Market : Harriet.Db.Market_Reference;
      Agent  : Harriet.Db.Agent.Agent_Type)
   is
      package Offer_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Harriet.Db.Market_Offer_Reference,
           Harriet.Db."=");
      Offers : Offer_Lists.List;
   begin
      for Offer of
        Harriet.Db.Market_Offer.Select_By_Agent_Offer
          (Market, Agent.Reference)
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

end Harriet.Markets;
