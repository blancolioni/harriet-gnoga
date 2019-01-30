with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Agents;

with Harriet.Db.Installation;
with Harriet.Db.Pop;

package body Harriet.Employment is

   type Contract_Record is
      record
         Employer : Harriet.Db.Agent_Reference;
         Employee : Harriet.Db.Agent_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Salary   : Harriet.Money.Price_Type;
      end record;

   package Contract_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Contract_Record);

   Current : Contract_Lists.List;

   procedure Execute
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type);

   --------------------------------
   -- Create_Employment_Contract --
   --------------------------------

   procedure Create_Employment_Contract
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type)
   is
   begin
      Current.Append ((Employer, Employee, Quantity, Salary));
   end Create_Employment_Contract;

   procedure Execute
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type)
   is
      use Harriet.Money;
      Installation : Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation (Employer);
      Pop          : Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_Pop (Employee);
      Employed_Pop : Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_By_Pop_Group_Installation
                         (Pop.Pop_Group, Installation.Reference);
      Cash_Per_Pop : constant Price_Type :=
                       Price (Harriet.Agents.Cash (Pop), Pop.Size);
   begin
      null;
   end Execute;

   ----------------------------------
   -- Execute_Employment_Contracts --
   ----------------------------------

   procedure Execute_Employment_Contracts is
   begin
      for Contract of Current loop
         Execute (Contract.Employer, Contract.Employee,
                  Contract.Quantity, Contract.Salary);
      end loop;
      Current.Clear;
   end Execute_Employment_Contracts;

end Harriet.Employment;
