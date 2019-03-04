with WL.Localisation;

with Harriet.Db.Commodity;
with Harriet.Db.Stock_Item;

package body Harriet.Commodities is

   function Get_Rec
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Stock_Record;

   ---------
   -- Add --
   ---------

   procedure Add
     (To    : in out Stock_Type;
      Other : Stock_Type)
   is
   begin
      for Rec of Other.List loop
         declare
            use Harriet.Money, Harriet.Quantities;
            To_Rec : constant Stock_Record :=
                       Get_Rec (To, Rec.Commodity);
         begin
            Set_Quantity (To, Rec.Commodity,
                          Rec.Quantity + To_Rec.Quantity,
                          Rec.Value + To_Rec.Value);
         end;
      end loop;
   end Add;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Price_Type
   is
      use Harriet.Quantities;
      Rec : constant Stock_Record := Stock.Get_Rec (Commodity);
   begin
      if Rec.Quantity = Zero then
         return Harriet.Money.Zero;
      else
         return Harriet.Money.Price (Rec.Value, Rec.Quantity);
      end if;
   end Get_Price_Per_Item;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Quantities.Quantity_Type
   is
      use type Harriet.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec.Quantity;
         end if;
      end loop;
      return Harriet.Quantities.Zero;
   end Get_Quantity;

   -------------
   -- Get_Rec --
   -------------

   function Get_Rec
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Stock_Record
   is
      use type Harriet.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec;
         end if;
      end loop;
      return (Commodity, Harriet.Quantities.Zero, Harriet.Money.Zero);
   end Get_Rec;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Stock     : Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference)
      return Harriet.Money.Money_Type
   is
      use type Harriet.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec.Value;
         end if;
      end loop;
      return Harriet.Money.Zero;
   end Get_Value;

   -------------------
   -- Initial_Price --
   -------------------

   function Initial_Price
     (Commodity : Harriet_Commodity)
      return Harriet.Money.Price_Type
   is
   begin
      return Harriet.Db.Commodity.Get (Commodity).Initial_Price;
   end Initial_Price;

   ------------------
   -- Is_Pop_Group --
   ------------------

   function Is_Pop_Group
     (Commodity : Harriet_Commodity)
      return Boolean
   is
      use Harriet.Db;
   begin
      return Harriet.Db.Commodity.Get (Commodity).Top_Record = R_Pop_Group;
   end Is_Pop_Group;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Stock   : Stock_Type;
      Process : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type;
                   Value     : Harriet.Money.Money_Type))
   is
      use Harriet.Quantities;
   begin
      for Rec of Stock.List loop
         if Rec.Quantity > Zero then
            Process (Rec.Commodity, Rec.Quantity, Rec.Value);
         end if;
      end loop;
   end Iterate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stock     : in out Stock_Type;
      Has_Stock : Harriet.Db.Has_Stock_Reference)
   is
      use Harriet.Quantities;
   begin
      for Item of Harriet.Db.Stock_Item.Select_By_Has_Stock (Has_Stock) loop
         if Item.Quantity > Zero then
            Stock.Set_Quantity
              (Item.Commodity, Item.Quantity, Item.Value);
         end if;
      end loop;
   end Load;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name
     (Commodity : Harriet_Commodity)
      return String
   is
      Tag : constant String :=
              Harriet.Db.Commodity.Get (Commodity).Tag;
   begin
      if WL.Localisation.Has_Local_Text (Tag) then
         return WL.Localisation.Local_Text (Tag);
      else
         return Tag;
      end if;
   end Local_Name;

   -------------
   -- Missing --
   -------------

   function Missing
     (Available_Stock : Stock_Type;
      Required_Stock  : Stock_Type)
      return Stock_Type
   is
   begin
      return Stock : Stock_Type do
         for Rec of Required_Stock.List loop
            declare
               use Harriet.Money, Harriet.Quantities;
               Available : constant Stock_Record :=
                             Get_Rec (Available_Stock, Rec.Commodity);
               Missing   : constant Quantity_Type :=
                             (if Available.Quantity < Rec.Quantity
                              then Rec.Quantity - Available.Quantity
                              else Zero);
               Value     : constant Harriet.Money.Money_Type :=
                             Harriet.Money.Total
                               (Harriet.Money.Price
                                  (Available.Value + Rec.Value,
                                   Available.Quantity + Rec.Quantity),
                                Missing);
            begin
               if Missing > Zero and then Value > Zero then
                  Stock.Set_Quantity
                    (Rec.Commodity, Missing, Value);
               end if;
            end;
         end loop;
      end return;
   end Missing;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Value     : Harriet.Money.Money_Type)
   is
      use Harriet.Money, Harriet.Quantities;
      use type Harriet.Db.Commodity_Reference;
   begin
      pragma Assert (Value > Zero or else Quantity = Zero);
      Stock.Total_Quantity := Stock.Total_Quantity + Quantity;
      Stock.Total_Value := Stock.Total_Value + Value;

      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            Stock.Total_Quantity := Stock.Total_Quantity - Rec.Quantity;
            Stock.Total_Value := Stock.Total_Value - Rec.Value;
            Rec.Quantity := Quantity;
            Rec.Value    := Value;
            return;
         end if;
      end loop;
      Stock.List.Append ((Commodity, Quantity, Value));
   end Set_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Harriet.Db.Commodity_Reference;
      Quantity  : Harriet.Quantities.Quantity_Type;
      Price_Per : Harriet.Money.Price_Type)
   is
   begin
      Stock.Set_Quantity (Commodity, Quantity,
                          Harriet.Money.Total (Price_Per, Quantity));
   end Set_Quantity;

   --------------------
   -- Total_Quantity --
   --------------------

   function Total_Quantity
     (Stock     : Stock_Type)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Stock.Total_Quantity;
   end Total_Quantity;

   -----------------
   -- Total_Value --
   -----------------

   function Total_Value
     (Stock     : Stock_Type)
      return Harriet.Money.Money_Type
   is
   begin
      return Stock.Total_Value;
   end Total_Value;

end Harriet.Commodities;
