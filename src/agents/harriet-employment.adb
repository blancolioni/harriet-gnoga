with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Calendar;
with Harriet.Pops;
with Harriet.Updates.Events;

with Harriet.Db.Installation;
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
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type);

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
     (Employer : Harriet.Db.Agent_Reference;
      Employee : Harriet.Db.Agent_Reference;
      Quantity : Harriet.Quantities.Quantity_Type;
      Salary   : Harriet.Money.Price_Type)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation (Employer);
      Pop          : constant Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_Pop (Employee);
      Employed_Pop : constant Harriet.Db.Pop.Pop_Type :=
                       Harriet.Db.Pop.Get_By_Pop_Group_Installation
                         (Pop.Pop_Group,
                          Installation.Get_Installation_Reference);
   begin
      if Employed_Pop.Has_Element then
         Employed_Pop.Set_Salary (Salary);
         Harriet.Pops.Move_Pops (Pop, Employed_Pop, Quantity);
      else
         declare
            New_Pop : constant Harriet.Db.Pop.Pop_Type :=
                        Harriet.Pops.New_Empty_Pop
                          (Pop.Faction, Pop.Pop_Group,
                           Installation.World,
                           Installation.World_Sector);
         begin
            New_Pop.Set_Active (True);
            New_Pop.Set_Next_Event (Pop.Next_Event);
            New_Pop.Set_Installation (Installation);
            Harriet.Pops.Move_Pops (Pop, New_Pop, Quantity);
         end;
      end if;
   end Execute;

end Harriet.Employment;
