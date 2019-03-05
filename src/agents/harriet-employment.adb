with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Calendar;
with Harriet.Updates.Events;

with Harriet.Agents;
with Harriet.Pops;

with Harriet.Db.Employer;
with Harriet.Db.Pop;

package body Harriet.Employment is

   Employment_Updates_Started : Boolean := False;

   type Employment_Contracts_Update is
     new Harriet.Updates.Update_Interface with null record;

   overriding procedure Activate
     (Item : Employment_Contracts_Update);

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
     (Employer_Agent : Harriet.Db.Agent_Reference;
      Employee_Agent : Harriet.Db.Agent_Reference;
      Quantity       : Harriet.Quantities.Quantity_Type;
      Salary         : Harriet.Money.Price_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Item : Employment_Contracts_Update)
   is
   begin
      for Contract of Current loop
         Execute (Contract.Employer, Contract.Employee,
                  Contract.Quantity, Contract.Salary);
      end loop;
      Current.Clear;
      Harriet.Updates.Events.Update_With_Delay
        (Harriet.Calendar.Days (1.0),
         Item);
   end Activate;

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
      if not Employment_Updates_Started then
         declare
            use type Harriet.Calendar.Time;
            Update : Employment_Contracts_Update;
         begin
            Harriet.Updates.Events.Update_At
              (Clock  => Harriet.Calendar.Clock + Harriet.Calendar.Days (1.0),
               Update => Update);
            Employment_Updates_Started := True;
         end;
      end if;
   end Create_Employment_Contract;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Employer_Agent : Harriet.Db.Agent_Reference;
      Employee_Agent : Harriet.Db.Agent_Reference;
      Quantity       : Harriet.Quantities.Quantity_Type;
      Salary         : Harriet.Money.Price_Type)
   is
      Employer : constant Harriet.Db.Employer_Reference :=
                   Harriet.Db.Employer.Get_Employer (Employer_Agent)
                   .Get_Employer_Reference;
      Pop      : constant Harriet.Db.Pop.Pop_Type :=
        Harriet.Db.Pop.Get_Pop (Employee_Agent);
   begin

      if Harriet.Db.Pop.Is_Pop_Group_Employer (Pop.Pop_Group, Employer) then
         declare
            Employed : constant Harriet.Db.Pop.Pop_Type :=
                         Harriet.Db.Pop.Get_By_Pop_Group_Employer
                           (Pop.Pop_Group, Employer);
         begin
            Harriet.Pops.Move_Pops (Pop, Employed, Quantity);
            Employed.Set_Salary (Salary);
            Harriet.Agents.Log_Agent
              (Employed.Get_Agent_Reference,
               "executing employment contract: quantity "
               & Harriet.Quantities.Show (Quantity)
               & "; salary "
               & Harriet.Money.Show (Salary)
               & "; employer"
               & Harriet.Db.To_String (Employer));
         end;
      else
         declare
            Employed : constant Harriet.Db.Pop.Pop_Type :=
                         Harriet.Pops.New_Empty_Pop
                           (Faction => Pop.Faction,
                            Group   => Pop.Pop_Group,
                            World   => Pop.World,
                            Sector  => Pop.World_Sector);
         begin
            Harriet.Pops.Move_Pops (Pop, Employed, Quantity);
            Employed.Set_Salary (Salary);
            Employed.Set_Employer (Employer);
            Harriet.Agents.Log_Agent
              (Employed.Get_Agent_Reference,
               "executing employment contract: quantity "
               & Harriet.Quantities.Show (Quantity)
               & "; salary "
               & Harriet.Money.Show (Salary)
               & "; employer"
               & Harriet.Db.To_String (Employer));
         end;
      end if;
   end Execute;

end Harriet.Employment;
