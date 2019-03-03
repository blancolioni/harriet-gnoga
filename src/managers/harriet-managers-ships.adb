with Harriet.Db.Ship;

package body Harriet.Managers.Ships is

   type Ship_Trade_Manager is
     new Root_Ship_Manager with
      record
         null;
      end record;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Ship_Manager)
   is
   begin
      Harriet.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;
   end Create_Market_Offers;

   --------------------------
   -- Create_Trade_Manager --
   --------------------------

   function Create_Trade_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Ship    : constant Harriet.Db.Ship.Ship_Type :=
                  Harriet.Db.Ship.Get_Ship (Managed);
      Manager : Ship_Trade_Manager := Ship_Trade_Manager'
        (Harriet.Managers.Agents.Root_Agent_Manager with
         Ship => Ship.Get_Ship_Reference);
   begin
      Manager.Initialize_Agent_Manager (Ship, Ship.World);
      return new Ship_Trade_Manager'(Manager);
   end Create_Trade_Manager;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Ship_Manager)
   is
   begin
      null;
   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Ship_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
   begin
      null;
   end Get_Required_Stock;

end Harriet.Managers.Ships;
