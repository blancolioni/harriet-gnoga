private with Harriet.Managers.Agents;
private with Harriet.Commodities;
private with Harriet.Quantities;

package Harriet.Managers.Ships is

   function Create_Trade_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Ship_Manager is
     abstract new Harriet.Managers.Agents.Root_Agent_Manager with
      record
         Ship           : Harriet.Db.Ship_Reference;
         Current_World  : Harriet.Db.World_Reference;
         Current_System : Harriet.Db.Star_System_Reference;
         Cargo_Space    : Harriet.Quantities.Quantity_Type;
      end record;

   overriding function Identifier
     (Manager : Root_Ship_Manager)
      return String
   is ("ship" & Harriet.Db.To_String (Manager.Ship) & " manager");

   overriding function Managed_Object_Id
     (Manager : Root_Ship_Manager)
      return String
   is ("ship" & Harriet.Db.To_String (Manager.Ship));

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Ship_Manager);

   overriding procedure Get_Required_Stock
     (Manager : Root_Ship_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Ship_Manager);

end Harriet.Managers.Ships;
