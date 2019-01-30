with Harriet.Money;

with Harriet.Db.Agent;

package Harriet.Agents is

   function Cash
     (Account : Harriet.Db.Account_Reference)
      return Harriet.Money.Money_Type;

   function Cash
     (Agent : Harriet.Db.Agent.Agent_Type)
      return Harriet.Money.Money_Type;

   procedure Add_Cash
     (Agent : Harriet.Db.Agent.Agent_Type;
      Cash  : Harriet.Money.Money_Type);

   procedure Add_Cash
     (Account : Harriet.Db.Account_Reference;
      Cash    : Harriet.Money.Money_Type);

   procedure Remove_Cash
     (Account : Harriet.Db.Account_Reference;
      Cash    : Harriet.Money.Money_Type);

end Harriet.Agents;
