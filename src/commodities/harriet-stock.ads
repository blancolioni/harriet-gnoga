with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Has_Stock;

package Harriet.Stock is

   procedure Add_Stock
     (To       : Harriet.Db.Has_Stock.Has_Stock_Type;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type);

   procedure Add_Initial_Stock
     (To       : Harriet.Db.Has_Stock_Reference;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type);

   procedure Log_Stock
     (Stock : Harriet.Db.Has_Stock_Reference);

end Harriet.Stock;
