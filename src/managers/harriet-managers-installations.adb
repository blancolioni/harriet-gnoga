with Ada.Text_IO;

with Harriet.Logging;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Stock;

with Harriet.Markets;
with Harriet.Worlds;

with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Worker;
with Harriet.Db.Generated_Resource;
with Harriet.Db.Installation;
with Harriet.Db.Market;
with Harriet.Db.Pop_Group;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Generator;
with Harriet.Db.World_Sector;

package body Harriet.Managers.Installations is

   type Resource_Generator_Manager is
     new Root_Installation_Manager with
      record
         Rgen : Harriet.Db.Resource_Generator_Reference;
      end record;

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager);

   type Hub_Manager is
     new Root_Installation_Manager with
      record
         null;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Hub_Manager)
   is null;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation
                         (Managed);
      Facility     : constant Harriet.Db.Facility.Facility_Type :=
                       Harriet.Db.Facility.Get
                         (Installation.Facility);
   begin
      case Facility.Top_Record is
         when Harriet.Db.R_Resource_Generator =>
            Harriet.Logging.Log
              (Actor    => Facility.Tag,
               Location => Harriet.Worlds.Name (Installation.World),
               Category => "manager",
               Message  => "starting");

            declare
               Rgen : constant Harriet.Db.Resource_Generator_Reference :=
                        Harriet.Db.Resource_Generator.Get_Resource_Generator
                          (Installation.Facility).Reference;
               Manager : Resource_Generator_Manager :=
                           (Harriet.Managers.Agents.Root_Agent_Manager with
                            Installation => Installation.Reference,
                            Facility        => Installation.Facility,
                            Rgen            => Rgen);
            begin
               Manager.Initialize_Agent_Manager
                 (Installation, Installation.World);
               return new Resource_Generator_Manager'
                 (Manager);
            end;
         when others =>
            Ada.Text_IO.Put_Line
              ("warning: "
               & "no manager for facility "
               & Facility.Tag);
            return null;
      end case;
   end Create_Default_Manager;

   ------------------------
   -- Create_Hub_Manager --
   ------------------------

   function Create_Hub_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation
                         (Managed);
      Manager      : Hub_Manager :=
                       Hub_Manager'
                         (Harriet.Managers.Agents.Root_Agent_Manager with
                          Installation => Installation.Reference,
                          Facility     => Installation.Facility);
   begin
      Manager.Initialize_Agent_Manager
        (Installation, Installation.World);
      return new Hub_Manager'(Manager);
   end Create_Hub_Manager;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);

      Market : constant Harriet.Db.Market_Reference :=
                 Harriet.Db.Market.Get_Reference_By_World
                   (Harriet.Db.World_Sector.Get (Installation.World_Sector)
                    .World);

      procedure Add_Offers
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type);

      ----------------
      -- Add_Offers --
      ----------------

      procedure Add_Offers
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type)
      is
      begin
         Harriet.Markets.Ask
           (Market    => Market,
            Agent     => Installation,
            Commodity => Item,
            Quantity  => Quantity,
            Price     =>
              Harriet.Money.Adjust_Price
                (Harriet.Money.Price (Value, Quantity), 1.1));
      end Add_Offers;

   begin
      Create_Market_Offers (Root_Installation_Manager (Manager));

      Harriet.Stock.Scan_Stock
        (Installation, Add_Offers'Access);
   end Create_Market_Offers;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);
      Gen          : constant Harriet.Db.Resource_Generator
        .Resource_Generator_Type :=
          Harriet.Db.Resource_Generator.Get
            (Manager.Rgen);

   begin

      for Deposit of
        Harriet.Db.Deposit.Select_By_World_Sector
          (Installation.World_Sector)
      loop
         declare
            Resource : constant Harriet.Db.Resource_Reference :=
                         Deposit.Resource;
         begin
            if Harriet.Db.Generated_Resource.Is_Generated_Resource
              (Gen.Reference, Resource)
            then
               declare
                  use Harriet.Quantities;
                  Commodity : constant Harriet.Db.Commodity_Reference :=
                                Harriet.Db.Resource.Get (Resource).Reference;
                  Quantity : constant Quantity_Type :=
                               To_Quantity
                                 (Deposit.Accessibility * 20.0
                                  * (Harriet.Random.Unit_Random + 0.5));
                  Cost     : constant Harriet.Money.Money_Type :=
                               Harriet.Money.Total
                                 (Manager.Current_Market_Bid_Price
                                    (Commodity),
                                  Quantity);
               begin
                  Harriet.Stock.Add_Stock
                    (Installation, Commodity, Quantity, Cost);
               end;
            end if;
         end;
      end loop;

      Harriet.Stock.Log_Stock (Installation.Reference);

   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
   begin
      for Employee of
        Harriet.Db.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            Commodity : constant Harriet.Db.Commodity_Reference :=
                          Harriet.Db.Pop_Group.Get (Employee.Pop_Group)
                          .Reference;
            Quantity  : constant Harriet.Quantities.Quantity_Type :=
                          Harriet.Quantities.Scale
                            (Employee.Quantity, Manager.Capacity);
            Price     : constant Harriet.Money.Price_Type :=
                          Manager.Current_Market_Ask_Price (Commodity);
         begin
            Stock.Set_Quantity
              (Commodity, Quantity, Harriet.Money.Total (Price, Quantity));
         end;
      end loop;
   end Get_Required_Stock;

end Harriet.Managers.Installations;
