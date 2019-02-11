with Harriet.Commodities;
with Harriet.Logging;
with Harriet.Quantities;
with Harriet.Real_Images;
with Harriet.Stock;

with Harriet.Db.Account;

package body Harriet.Agents is

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Agent : Harriet.Db.Agent.Agent_Type;
      Cash  : Harriet.Money.Money_Type)
   is
      use type Harriet.Money.Money_Type;
      Account : constant Harriet.Db.Account.Account_Type :=
                  Harriet.Db.Account.Get (Agent.Account);
   begin
      Account.Set_Cash (Account.Cash + Cash);
   end Add_Cash;

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Account : Harriet.Db.Account_Reference;
      Cash    : Harriet.Money.Money_Type)
   is
      use type Harriet.Money.Money_Type;
      Rec : constant Harriet.Db.Account.Account_Type :=
              Harriet.Db.Account.Get (Account);
   begin
      Rec.Set_Cash (Rec.Cash + Cash);
   end Add_Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Account : Harriet.Db.Account_Reference)
      return Harriet.Money.Money_Type
   is
   begin
      return Harriet.Db.Account.Get (Account).Cash;
   end Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Harriet.Db.Agent.Agent_Type)
      return Harriet.Money.Money_Type
   is
   begin
      return Cash (Agent.Account);
   end Cash;

   ---------------
   -- Log_Agent --
   ---------------

   procedure Log_Agent
     (Agent   : Harriet.Db.Agent_Reference;
      Message : String)
   is
   begin
      Harriet.Logging.Log
        (Actor    => "Agent" & Harriet.Db.To_String (Agent),
         Location => "",
         Category => "",
         Message  => Message);
   end Log_Agent;

   -----------------
   -- Move_Assets --
   -----------------

   procedure Move_Assets
     (From     : Harriet.Db.Agent.Agent_Type;
      To       : Harriet.Db.Agent.Agent_Type;
      Fraction : Unit_Real)
   is
      use Harriet.Money;

      From_Account : constant Harriet.Db.Account.Account_Type :=
                       Harriet.Db.Account.Get (From.Account);
      To_Account   : constant Harriet.Db.Account.Account_Type :=
                       Harriet.Db.Account.Get (To.Account);
      Transfer_Cash : constant Money_Type :=
                        Adjust (From_Account.Cash, Fraction);
      From_Stock    : Harriet.Commodities.Stock_Type;

      procedure Transfer_Stock
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type);

      --------------------
      -- Transfer_Stock --
      --------------------

      procedure Transfer_Stock
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Value     : Harriet.Money.Money_Type)
      is
         use Harriet.Quantities;
         Transfer_Quantity : constant Quantity_Type :=
                               Scale (Quantity, Fraction);
         Transfer_Value    : constant Money_Type :=
                               Adjust (Value, Fraction);
      begin
         if not Harriet.Commodities.Is_Pop_Group (Commodity) then
            Log_Agent
              (From.Get_Agent_Reference,
               "transferring " & Show (Transfer_Quantity)
               & " " & Harriet.Commodities.Local_Name (Commodity)
               & " of " & Show (Quantity));
            Harriet.Stock.Remove_Stock
              (From.Get_Has_Stock_Reference, Commodity, Transfer_Quantity);
            Harriet.Stock.Add_Stock
              (To.Get_Has_Stock_Reference,
               Commodity, Transfer_Quantity, Transfer_Value);
         end if;
      end Transfer_Stock;

   begin

      Log_Agent (From.Get_Agent_Reference,
                 "transferring "
                 & Harriet.Real_Images.Approximate_Image (Fraction * 100.0)
                 & "% to agent"
                 & Harriet.Db.To_String
                   (To.Get_Agent_Reference));

      From_Account.Set_Cash (From_Account.Cash - Transfer_Cash);
      To_Account.Set_Cash (To_Account.Cash + Transfer_Cash);

      From_Stock.Load (From.Get_Has_Stock_Reference);
      From_Stock.Iterate (Transfer_Stock'Access);

   exception
      when others =>
         Log_Agent (From.Get_Agent_Reference,
                    "exception during transfer");
         raise;
--        for Stock of
--          Harriet.Db.Stock_Item.Select_By_Has_Stock (From.Reference)
--        loop
--           declare
--              use Harriet.Quantities;
--              Current_Quantity : constant Quantity_Type :=
--                                   Stock.Quantity;
--              Transfer_Quantity : constant Quantity_Type :=
--                                    Scale (Current_Quantity, Fraction);
--              Current_Value     : constant Money_Type :=
--                                    Stock.Value;
--              Transfer_Value    : constant Money_Type :=
--                                    Adjust (Current_Value, Fraction);
--           begin
--              Stock.Set_Quantity (Current_Quantity - Transfer_Quantity);
--              Stock.Set_Value (Current_Value - Transfer_Value);
--              Harriet.Stock.Add_Stock
--                (To.Reference, Stock.Commodity,
--                 Transfer_Quantity, Transfer_Value);
--           end;
--        end loop;
   end Move_Assets;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Account : Harriet.Db.Account_Reference;
      Cash    : Harriet.Money.Money_Type)
   is
      use type Harriet.Money.Money_Type;
      Rec : constant Harriet.Db.Account.Account_Type :=
              Harriet.Db.Account.Get (Account);
   begin
      Rec.Set_Cash (Rec.Cash - Cash);
   end Remove_Cash;

end Harriet.Agents;
