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

end Harriet.Agents;
