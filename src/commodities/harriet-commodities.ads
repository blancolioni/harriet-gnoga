with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db;

package Harriet.Commodities is

   subtype Harriet_Commodity is Harriet.Db.Commodity_Reference;

   function Initial_Price
     (Commodity : Harriet_Commodity)
      return Harriet.Money.Price_Type;

   type Stock_Type is tagged private;

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type);

   function Total_Quantity
     (Stock     : Stock_Type)
      return Harriet.Quantities.Quantity_Type;

   procedure Iterate
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type));

   function Missing
     (Available_Stock : Stock_Type;
      Required_Stock  : Stock_Type)
      return Stock_Type;

   procedure Add
     (To    : in out Stock_Type;
      Other : Stock_Type);

   procedure Load
     (Stock     : in out Stock_Type;
      Has_Stock : Harriet.Db.Has_Stock_Reference);

private

   type Stock_Type is tagged null record;

end Harriet.Commodities;
