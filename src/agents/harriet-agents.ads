with Harriet.Money;

with Harriet.Db.Agent;

package Harriet.Agents is

   procedure Add_Cash
     (Agent : Harriet.Db.Agent.Agent_Type;
      Cash  : Harriet.Money.Money_Type);

end Harriet.Agents;
