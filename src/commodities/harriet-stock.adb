with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Logging;

with Harriet.Commodities;

with Harriet.Db.Commodity;
with Harriet.Db.Historical_Stock;
with Harriet.Db.Stock_Item;

package body Harriet.Stock is

   Log_Stock_Enabled : constant Boolean := False;

   procedure Register_Stock
     (Stock     : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference);

   -----------------------
   -- Add_Initial_Stock --
   -----------------------

   procedure Add_Initial_Stock
     (To       : Harriet.Db.Has_Stock_Reference;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type)
   is
   begin
      Add_Stock (To, Item, Quantity,
                 Harriet.Money.Total
                   (Harriet.Db.Commodity.Get (Item).Initial_Price,
                    Quantity));
   end Add_Initial_Stock;

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
   begin
      if Quantity > Zero then
         pragma Assert (Value > Zero);
         Add_Stock (To.Get_Has_Stock_Reference, Item, Quantity, Value);
      end if;
   end Add_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To       : Harriet.Db.Has_Stock_Reference;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type)
   is
      use Harriet.Money, Harriet.Quantities;
   begin

      if Quantity > Zero then
         pragma Assert (Value > Zero);
         declare
            Stock : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
                      Harriet.Db.Stock_Item.Get_By_Stock_Item
                        (To, Item);
         begin
            if Stock.Has_Element then
               Stock.Set_Quantity (Stock.Quantity + Quantity);
               Stock.Set_Value (Stock.Value + Value);
            else
               Harriet.Db.Stock_Item.Create
                 (Has_Stock => To,
                  Commodity => Item,
                  Quantity  => Quantity,
                  Value     => Value);
            end if;
         end;
         Register_Stock (To, Item);
      end if;

   end Add_Stock;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Harriet.Money.Price (Value, Quantity);
   end Get_Price_Per_Item;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Quantity;
   end Get_Quantity;

   ---------------
   -- Get_Stock --
   ---------------

   procedure Get_Stock
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : out Harriet.Quantities.Quantity_Type;
      Value     : out Harriet.Money.Money_Type)
   is
      Stock : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
                Harriet.Db.Stock_Item.Get_By_Stock_Item
                  (Has_Stock, Commodity);
   begin
      if Stock.Has_Element then
         Quantity := Stock.Quantity;
         Value := Stock.Value;
      else
         Quantity := Quantities.Zero;
         Value := Money.Zero;
      end if;
   end Get_Stock;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Money_Type
   is
      Quantity : Harriet.Quantities.Quantity_Type;
      Value    : Harriet.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Value;
   end Get_Value;

   ---------------
   -- Log_Stock --
   ---------------

   procedure Log_Stock
     (Stock : Harriet.Db.Has_Stock_Reference)
   is
      Actor : constant String :=
                "has-stock" & Harriet.Db.To_String (Stock);
   begin
      if Log_Stock_Enabled then
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
      end if;
   end Log_Stock;

   --------------------
   -- Register_Stock --
   --------------------

   procedure Register_Stock
     (Stock     : Harriet.Db.Has_Stock_Reference;
      Commodity : Harriet.Db.Commodity_Reference)
   is
      Clock : constant Harriet.Calendar.Time :=
                Harriet.Calendar.Clock;
      Historical : constant Db.Historical_Stock.Historical_Stock_Type :=
                     Db.Historical_Stock.Get_By_Historical_Stock
                       (Stock, Commodity, Clock);
      Quantity   : Harriet.Quantities.Quantity_Type;
      Value      : Harriet.Money.Money_Type;
   begin
      Get_Stock (Stock, Commodity, Quantity, Value);

      if Historical.Has_Element then
         Historical.Set_Quantity (Quantity);
         Historical.Set_Value (Value);
      else
         Harriet.Db.Historical_Stock.Create
           (Time_Stamp => Clock,
            Has_Stock  => Stock,
            Commodity  => Commodity,
            Quantity   => Quantity,
            Value      => Value);
      end if;
   end Register_Stock;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From     : Harriet.Db.Has_Stock_Reference;
      Item     : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Money, Harriet.Quantities;
   begin

      if Quantity > Zero then
         declare
            Stock : constant Harriet.Db.Stock_Item.Stock_Item_Type :=
                      Harriet.Db.Stock_Item.Get_By_Stock_Item
                        (From, Item);
            Available : constant Quantity_Type := Stock.Quantity;
            Value : Money_Type;
         begin
            if Available < Quantity then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "remove-stock: "
                  & Harriet.Commodities.Local_Name (Item)
                  & ": attempt to remove "
                  & Image (Quantity) & " but have only "
                  & Image (Available));
            end if;

            pragma Assert (Stock.Has_Element);
            pragma Assert (Available >= Quantity);

            Value := Total (Price (Stock.Value, Stock.Quantity), Quantity);

            if Value = Stock.Value and then Quantity < Stock.Quantity then
               Value := Stock.Value - To_Money (0.01);
            end if;
            Stock.Set_Quantity (Stock.Quantity - Quantity);
            Stock.Set_Value (Stock.Value - Value);
         end;
         Register_Stock (From, Item);
      end if;

   end Remove_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Has_Stock : Harriet.Db.Has_Stock_Reference;
      Process   : not null access
        procedure (Item     : Harriet.Db.Commodity_Reference;
                   Quantity : Harriet.Quantities.Quantity_Type;
                   Value    : Harriet.Money.Money_Type))
   is
      Stock : Harriet.Commodities.Stock_Type;
   begin
      Stock.Load (Has_Stock);
      Stock.Iterate (Process);
   end Scan_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Has_Stock : Harriet.Db.Has_Stock.Has_Stock_Type;
      Process   : not null access
        procedure (Item     : Harriet.Db.Commodity_Reference;
                   Quantity : Harriet.Quantities.Quantity_Type;
                   Value    : Harriet.Money.Money_Type))
   is
   begin
      Scan_Stock (Has_Stock.Get_Has_Stock_Reference, Process);
   end Scan_Stock;

end Harriet.Stock;
