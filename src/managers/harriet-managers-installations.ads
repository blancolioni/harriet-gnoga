with Harriet.Commodities;

package Harriet.Managers.Installations is

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

   function Create_Hub_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Installation_Manager is
     abstract new Root_Manager_Type with
      record
         Installation : Harriet.Db.Installation_Reference;
         Agent        : Harriet.Db.Agent_Reference;
         Has_Stock    : Harriet.Db.Has_Stock_Reference;
         Market       : Harriet.Db.Market_Reference;
         Account      : Harriet.Db.Account_Reference;
         Facility     : Harriet.Db.Facility_Reference;
         Capacity     : Unit_Real;
      end record;

   overriding procedure Activate
     (Manager : not null access Root_Installation_Manager);

   procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type);

   function Calculate_Capacity
     (Manager : Root_Installation_Manager;
      Stock   : Harriet.Commodities.Stock_Type)
     return Unit_Real;

end Harriet.Managers.Installations;
