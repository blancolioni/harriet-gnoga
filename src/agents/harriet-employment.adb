with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Agents;
with Harriet.Calendar;

with Harriet.Db.Account;
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
      use Harriet.Money, Harriet.Quantities;
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation (Employer);
      Pop          : constant Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_Pop (Employee);
      Account      : constant Harriet.Db.Account.Account_Type :=
                       Harriet.Db.Account.Get (Pop.Account);
      Employed_Pop : constant Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_By_Pop_Group_Installation
                         (Pop.Pop_Group, Installation.Reference);
      Cash_Per_Pop : constant Price_Type :=
                       Price (Account.Cash, Pop.Size);
      New_Pop_Size : constant Quantity_Type := Pop.Size - Quantity;
      New_Account  : Harriet.Db.Account_Reference;
   begin
      Account.Set_Cash (Account.Cash - Total (Cash_Per_Pop, Quantity));
      Pop.Set_Size (New_Pop_Size);
      if Employed_Pop.Has_Element then
         New_Account := Employed_Pop.Account;
         Employed_Pop.Set_Size (Employed_Pop.Size + Quantity);
      else
         New_Account :=
           Harriet.Db.Account.Create
             (Harriet.Db.Null_Account_Reference, Zero, Zero);
         Harriet.Db.Pop.Create
           (Transported_Size => 0.0,
            Active           => True,
            Next_Event       => Harriet.Calendar.Clock,
            Manager          => "default-pop",
            Account          => New_Account,
            Capacity         => Zero,
            Faction          => Pop.Faction,
            World            => Installation.World,
            World_Sector     => Installation.World_Sector,
            Installation     => Installation.Reference,
            Ship             => Harriet.Db.Null_Ship_Reference,
            Pop_Group        => Pop.Pop_Group,
            Size             => Quantity,
            Salary           => Salary,
            Happiness        => Pop.Happiness);
      end if;

      Harriet.Agents.Add_Cash (New_Account, Total (Cash_Per_Pop, Quantity));

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
