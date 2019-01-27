with Ada.Text_IO;

with Harriet.Logging;

with Harriet.Db.Commodity;
with Harriet.Db.Stock_Item;

package body Harriet.Stock is

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To       : Harriet.Db.Has_Stock.Has_Stock_Type;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type)
   is
      use Harriet.Money, Harriet.Quantities;
      Stock : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
                Harriet.Db.Stock_Item.Get_By_Stock_Item
                  (To.Reference, Item);
   begin
      if False then
         Ada.Text_IO.Put_Line
           ("add "
            & Harriet.Quantities.Show (Quantity)
            & " "
            & Harriet.Db.Commodity.Get (Item).Tag
            & " to "
            & Harriet.Db.Record_Type'Image (To.Top_Record)
            & Harriet.Db.To_String
              (Harriet.Db.Has_Stock_Reference'(To.Reference)));
      end if;

      if Stock.Has_Element then
         Stock.Set_Quantity (Stock.Quantity + Quantity);
         Stock.Set_Value (Stock.Value + Value);
      else
         Harriet.Db.Stock_Item.Create
           (Has_Stock => To.Reference,
            Commodity => Item,
            Quantity  => Quantity,
            Value     => Value);
      end if;
   end Add_Stock;

   ---------------
   -- Log_Stock --
   ---------------

   procedure Log_Stock
     (Stock : Harriet.Db.Has_Stock_Reference)
   is
      Actor : constant String :=
                "has-stock" & Harriet.Db.To_String (Stock);
   begin
      for Stock_Item of
        Harriet.Db.Stock_Item.Select_By_Has_Stock
          (Stock)
      loop
         Harriet.Logging.Log
           (Actor    => Actor,
            Location => "",
            Category => "stock",
            Message  =>
              Harriet.Db.Commodity.Get (Stock_Item.Commodity).Tag
                & " "
            & Harriet.Quantities.Show (Stock_Item.Quantity));
      end loop;
   end Log_Stock;

end Harriet.Stock;
