with Ada.Text_IO;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Stock;

with Harriet.Markets;

with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Generated_Resource;
with Harriet.Db.Installation;
with Harriet.Db.Market;
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
      Harriet.Markets.Reset_Offers (Market, Installation);

      Harriet.Stock.Scan_Stock
        (Installation, Add_Offers'Access);
   end Activate;

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

end Harriet.Managers.Installations;
