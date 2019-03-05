with Harriet.Commodities;

private with Harriet.Managers.Agents;
private with Harriet.Db.Facility;
private with Harriet.Money;

package Harriet.Managers.Installations is

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

   function Create_Hub_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Installation_Manager is
     abstract new Harriet.Managers.Agents.Root_Agent_Manager with
      record
         Employer     : Harriet.Db.Employer_Reference;
         Installation : Harriet.Db.Installation_Reference;
         Facility     : Harriet.Db.Facility_Reference;
         Payroll      : Harriet.Money.Money_Type;
      end record;

   overriding function Identifier
     (Manager : Root_Installation_Manager)
      return String
   is ("installation"
       & Harriet.Db.To_String (Manager.Installation) & " manager");

   overriding function Managed_Object_Id
     (Manager : Root_Installation_Manager)
      return String
   is (Harriet.Db.Facility.Get (Manager.Facility).Tag
       & Harriet.Db.To_String (Manager.Installation));

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Installation_Manager);

end Harriet.Managers.Installations;
