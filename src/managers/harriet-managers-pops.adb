with Harriet.Db.Pop;
with Harriet.Db.Pop_Group;

with Harriet.Db.Clothing_Commodity;
with Harriet.Db.Drink_Commodity;
with Harriet.Db.Food_Commodity;

package body Harriet.Managers.Pops is

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      use Harriet.Db;
      Pop     : constant Harriet.Db.Pop.Pop_Type :=
                  Harriet.Db.Pop.Get_Pop
                    (Managed);
      Group   : constant Harriet.Db.Pop_Group.Pop_Group_Type :=
                  Harriet.Db.Pop_Group.Get (Pop.Pop_Group);
      Manager : Root_Pop_Manager :=
                  (Harriet.Managers.Agents.Root_Agent_Manager with
                   Pop                 => Pop.Reference,
                   Group               => Pop.Pop_Group,
                   Commodity           => Group.Reference,
                   Employer            => Pop.Installation,
                   Employed            =>
                     Pop.Installation /= Null_Installation_Reference,
                   Consumption_Quality => Group.Consumer_Quality,
                   Service_Quality     => Group.Service_Quality);
   begin
      Manager.Initialize_Agent_Manager (Pop, Pop.World);
      return new Root_Pop_Manager'(Manager);
   end Create_Default_Manager;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Pop_Manager)
   is
   begin
      Harriet.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;
      if not Manager.Employed then
         Manager.Place_Ask
           (Commodity => Manager.Commodity,
            Quantity  =>
              Harriet.Db.Pop.Get (Manager.Pop).Size,
            Price     => Manager.Current_Market_Bid_Price (Manager.Commodity));
      end if;

   end Create_Market_Offers;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Pop_Manager)
   is
   begin
      null;
   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Pop_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
      Food : constant Harriet.Db.Food_Commodity.Food_Commodity_Type :=
               Harriet.Db.Food_Commodity.First_By_Quality
                 (Manager.Consumption_Quality);
      Drink : constant Harriet.Db.Drink_Commodity.Drink_Commodity_Type :=
                Harriet.Db.Drink_Commodity.First_By_Quality
                  (Manager.Consumption_Quality);
      Clothing : constant Harriet.Db.Clothing_Commodity
        .Clothing_Commodity_Type :=
          Harriet.Db.Clothing_Commodity.First_By_Quality
            (Manager.Consumption_Quality);
   begin
      Stock.Set_Quantity
        (Food.Reference, Manager.Size,
         Manager.Current_Market_Ask_Price (Food.Reference));
      Stock.Set_Quantity
        (Drink.Reference, Manager.Size,
         Manager.Current_Market_Ask_Price (Drink.Reference));
      Stock.Set_Quantity
        (Clothing.Reference, Manager.Size,
         Manager.Current_Market_Ask_Price (Clothing.Reference));
   end Get_Required_Stock;

   ----------
   -- Size --
   ----------

   function Size
     (Manager : Root_Pop_Manager'Class)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Db.Pop.Get (Manager.Pop).Size;
   end Size;

end Harriet.Managers.Pops;
