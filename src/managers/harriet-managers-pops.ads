with Harriet.Commodities;
private with Harriet.Quantities;

private with Harriet.Managers.Agents;

package Harriet.Managers.Pops is

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Pop_Manager is
     new Harriet.Managers.Agents.Root_Agent_Manager with
      record
         Pop                 : Harriet.Db.Pop_Reference;
         Group               : Harriet.Db.Pop_Group_Reference;
         Commodity           : Harriet.Db.Commodity_Reference;
         Employer            : Harriet.Db.Installation_Reference;
         Employed            : Boolean;
         Consumption_Quality : Positive;
         Service_Quality     : Positive;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Pop_Manager);

   overriding procedure Get_Required_Stock
     (Manager : Root_Pop_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Pop_Manager);

   function Size
     (Manager : Root_Pop_Manager'Class)
      return Harriet.Quantities.Quantity_Type;

end Harriet.Managers.Pops;
