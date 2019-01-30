with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db;

package Harriet.Employment is

   procedure Create_Employment_Contract
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type);

   procedure Execute_Employment_Contracts;

end Harriet.Employment;
