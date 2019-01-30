with Harriet.Commodities;

private with Harriet.Managers.Agents;

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
         Installation : Harriet.Db.Installation_Reference;
         Facility     : Harriet.Db.Facility_Reference;
      end record;

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type);

end Harriet.Managers.Installations;
