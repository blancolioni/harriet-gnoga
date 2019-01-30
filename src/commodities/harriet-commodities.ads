private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db;

package Harriet.Commodities is

   subtype Harriet_Commodity is Harriet.Db.Commodity_Reference;

   function Local_Name
     (Commodity : Harriet_Commodity)
      return String;

   function Initial_Price
     (Commodity : Harriet_Commodity)
      return Harriet.Money.Price_Type;

   function Is_Pop_Group
     (Commodity : Harriet_Commodity)
      return Boolean;

   type Stock_Type is tagged private;

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type;

   function Get_Value
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Money_Type;

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type;

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Value     : Harriet.Money.Money_Type);

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price_Per : Harriet.Money.Price_Type);

   function Total_Quantity
     (Stock     : Stock_Type)
      return Harriet.Quantities.Quantity_Type;

   function Total_Value
     (Stock     : Stock_Type)
      return Harriet.Money.Money_Type;

   procedure Iterate
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type;
                   Value     : Harriet.Money.Money_Type));

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

   type Stock_Record is
      record
         Commodity : Harriet_Commodity;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type;
      end record;

   package Stock_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Record);

   type Stock_Type is tagged
      record
         List           : Stock_Lists.List;
         Total_Quantity : Harriet.Quantities.Quantity_Type :=
                            Harriet.Quantities.Zero;
         Total_Value    : Harriet.Money.Money_Type :=
                            Harriet.Money.Zero;
      end record;

end Harriet.Commodities;
