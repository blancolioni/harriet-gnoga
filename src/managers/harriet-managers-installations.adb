with Ada.Text_IO;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Stock;

with Harriet.Agents;
with Harriet.Markets;

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

   overriding procedure Activate
     (Manager : not null access Resource_Generator_Manager);

   type Hub_Manager is
     new Root_Installation_Manager with
      record
         null;
      end record;

   overriding procedure Activate
     (Manager : not null access Hub_Manager);

   overriding procedure Activate
     (Manager : not null access Root_Installation_Manager)
   is
      use Harriet.Commodities;
      use Harriet.Money;
      use Harriet.Quantities;
      M         : Root_Installation_Manager'Class renames
                    Root_Installation_Manager'Class (Manager.all);
      Have      : Harriet.Commodities.Stock_Type;
      Required  : Harriet.Commodities.Stock_Type;
      Missing   : Harriet.Commodities.Stock_Type;
      Available : Harriet.Commodities.Stock_Type;
      Cost      : Harriet.Money.Money_Type;
      Cash      : constant Harriet.Money.Money_Type :=
                    Harriet.Agents.Cash (Manager.Account);

      procedure Place_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantity_Type);

      ---------------
      -- Place_Bid --
      ---------------

      procedure Place_Bid
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Quantity_Type)
      is
      begin
         Harriet.Markets.Bid
           (Market    => Manager.Market,
            Agent     => Manager.Agent,
            Account   => Manager.Account,
            Has_Stock => Manager.Has_Stock,
            Commodity => Commodity,
            Quantity  => Quantity,
            Price     =>
              Harriet.Markets.Current_Ask_Price
                (Manager.Market, Commodity));
      end Place_Bid;

   begin
      Have.Load (Manager.Has_Stock);
      Manager.Capacity := 1.0;
      M.Get_Required_Stock (Required);
      Missing := Harriet.Commodities.Missing (Have, Required);
      if Missing.Total_Quantity > Zero then
         Harriet.Markets.Try_Bid (Manager.Market, Missing, Available, Cost);
         Available.Add (Have);
         Manager.Capacity :=
           M.Calculate_Capacity (Available);
         if Cost > Cash then
            declare
               Base : constant Unit_Real :=
                        M.Calculate_Capacity (Have);
            begin
               Manager.Capacity :=
                 Base +
                   (Manager.Capacity - Base) * To_Real (Cash) / To_Real (Cost);
            end;
         end if;

         Missing := Have.Missing (Available);
         Missing.Iterate (Place_Bid'Access);
      end if;
   end Activate;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Resource_Generator_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);
      Gen          : constant Harriet.Db.Resource_Generator
        .Resource_Generator_Type :=
          Harriet.Db.Resource_Generator.Get
            (Manager.Rgen);

   begin

      Activate (Root_Installation_Manager (Manager.all)'Access);

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
                  Quantity : constant Quantity_Type :=
                               To_Quantity
                                 (Deposit.Accessibility * 20.0
                                  * (Harriet.Random.Unit_Random + 0.5));
                  Cost     : constant Harriet.Money.Money_Type :=
                               Harriet.Money.Zero;
               begin
                  Harriet.Stock.Add_Stock
                    (Installation,
                     Harriet.Db.Resource.Get (Resource).Reference,
                     Quantity,
                     Cost);
               end;
            end if;
         end;
      end loop;

      Harriet.Stock.Log_Stock (Installation.Reference);

      Manager.Set_Next_Update_Delay
        (Harriet.Calendar.Days (1));

   end Activate;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Hub_Manager)
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

      Activate (Root_Installation_Manager (Manager.all)'Access);

      Harriet.Markets.Reset_Offers (Market, Installation);

      Harriet.Stock.Scan_Stock
        (Installation, Add_Offers'Access);
   end Activate;

   ------------------------
   -- Calculate_Capacity --
   ------------------------

   function Calculate_Capacity
     (Manager : Root_Installation_Manager;
      Stock   : Harriet.Commodities.Stock_Type)
      return Unit_Real
   is
      Result : Unit_Real := 1.0;
   begin
      for Employee of
        Harriet.Db.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            use Harriet.Quantities;
            Commodity : constant Harriet.Db.Commodity_Reference :=
                          Harriet.Db.Pop_Group.Get
                            (Employee.Pop_Group).Reference;
            Required  : constant Quantity_Type :=
                          Employee.Quantity;
            Employed  : constant Quantity_Type :=
                          Stock.Get_Quantity (Commodity);
            Limit     : constant Non_Negative_Real :=
                          To_Real (Employed) / To_Real (Required);
         begin
            if Limit < Result then
               Result := Limit;
            end if;
         end;
      end loop;
      return Result;
   end Calculate_Capacity;

   ------------
   -- Create --
   ------------

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
            return new Resource_Generator_Manager'
              (Installation => Installation.Reference,
               Rgen         =>
                  Harriet.Db.Resource_Generator.Get_Resource_Generator
                    (Installation.Facility).Reference,
               others       => <>);
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
   begin
      return new Hub_Manager'
        (Installation    => Installation.Reference,
         others          => <>);
   end Create_Hub_Manager;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
   begin
      for Employee of
        Harriet.Db.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         Stock.Set_Quantity
           (Harriet.Db.Pop_Group.Get (Employee.Pop_Group).Reference,
            Harriet.Quantities.Scale (Employee.Quantity, Manager.Capacity));
      end loop;
   end Get_Required_Stock;

end Harriet.Managers.Installations;
