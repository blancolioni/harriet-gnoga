with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;
with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Real_Images;

with Harriet.Constants;

with Harriet.Worlds;

with Harriet.Db.Account;
with Harriet.Db.Commodity;
with Harriet.Db.Container_Component;
with Harriet.Db.Drive_Component;
with Harriet.Db.Faction;
with Harriet.Db.Ship_Component;
with Harriet.Db.Ship_Design;
with Harriet.Db.Ship_Module;
with Harriet.Db.Ship_Module_Design;
with Harriet.Db.World;

package body Harriet.Ships is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Owner  : Harriet.Db.Faction_Reference;
      World  : Harriet.Db.World_Reference;
      Design : Harriet.Db.Ship_Design_Reference;
      Name   : String)
   is
      World_Rec : constant Harriet.Db.World.World_Type :=
                    Harriet.Db.World.Get (World);
      Account      : constant Harriet.Db.Account_Reference :=
                       Harriet.Db.Account.Create
                         (Harriet.Db.Null_Account_Reference,
                          Harriet.Money.Zero,
                          Harriet.Money.Zero);
      Ship         : constant Harriet.Db.Ship_Reference :=
                    Harriet.Db.Ship.Create
                      (Name            => Name,
                       Capacity        =>
                         Harriet.Db.Ship_Design.Get (Design).Hold_Size,
                       Account         => Account,
                       Faction         => Owner,
                       Active          => True,
                       Next_Event      => Harriet.Calendar.Clock,
                       Manager         => "default",
                       Owner           =>
                         Harriet.Db.Faction.Get (Owner).Reference,
                       World           => World,
                       Star_System     => World_Rec.Star_System,
                       Orbit           =>
                         World_Rec.Radius
                       + (300.0 + 100.0 * Harriet.Random.Unit_Random)
                       * 1000.0,
                       Inclination     =>
                         Harriet.Random.Unit_Random * 10.0 - 5.0,
                       Start_Time      => Harriet.Calendar.Clock,
                       Start_Longitude => Harriet.Random.Unit_Random * 360.0,
                       Ship_Design     => Design,
                       Alive           => True,
                       Training        => 0.0,
                       Fuel            => 0.0,
                       Destination     => Harriet.Db.Null_World_Reference,
                       Progress        => 0.0,
                       Current_Order   => 0,
                       Cycle_Orders    => False);
   begin
      for Design_Component of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design
          (Design)
      loop
         Harriet.Db.Ship_Module.Create
           (Ship           => Ship,
            Ship_Component => Design_Component.Ship_Component,
            Crew           => 0,
            Condition      => 0.0,
            Tec_Level      => 0.0);
      end loop;

      if False then
         declare
            S : constant Ship_Type := Get (Ship);
         begin
            Ada.Text_IO.Put_Line
              (S.Name & " in orbit above "
               & Harriet.Worlds.Name (S.World)
               & ": altitude "
               & Harriet.Real_Images.Approximate_Image
                 ((S.Orbit - Harriet.Worlds.Radius (S.World)) / 1_000.0)
               & "km inclination "
               & Harriet.Real_Images.Approximate_Image
                 (S.Inclination)
               & " deg longitude "
               & Harriet.Real_Images.Approximate_Image
                 (S.Current_Longitude)
               & " E");
         end;
      end if;
   end Create_Ship;

   -----------------------
   -- Current_Longitude --
   -----------------------

   function Current_Longitude
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
      use Harriet.Calendar;
      use Harriet.Elementary_Functions;
      Orbit       : constant Non_Negative_Real := Ship.Orbit;
      World_Mass  : constant Non_Negative_Real :=
                     Harriet.Worlds.Mass (Ship.World);
      Period      : constant Non_Negative_Real :=
                     Sqrt (4.0 * Ada.Numerics.Pi * Orbit ** 3
                           / Harriet.Constants.Gravitational_Constant
                           / World_Mass);
      Start       : constant Non_Negative_Real :=
                     Harriet.Db.Ship.Get (Ship.Reference).Start_Longitude;
      Start_Time  : constant Time :=
                     Harriet.Db.Ship.Get (Ship.Reference).Start_Time;
      Now         : constant Time := Clock;
      Elapsed     : constant Duration := Now - Start_Time;
      Orbit_Count : constant Non_Negative_Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
                      Orbit_Count - Real'Truncation (Orbit_Count);
      Longitude   : Non_Negative_Real := Start + Partial * 360.0;
   begin
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      return Longitude;
   end Current_Longitude;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Dry_Mass;
   end Current_Mass;

   -------------------------
   -- Design_Cargo_Volume --
   -------------------------

   function Design_Cargo_Volume
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Volume : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db, Harriet.Db.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Container_Component then
               declare
                  use Harriet.Db.Container_Component;
                  Container : constant Container_Component_Type :=
                                Get_Container_Component
                                  (Module.Ship_Component);
               begin
                  if not Container.Liquid
                    and then not Container.Cryo
                  then
                     Volume := Volume
                       + Harriet.Quantities.To_Real
                       (Container.Capacity);
                  end if;
               end;
            end if;
         end;
      end loop;
      return Volume;
   end Design_Cargo_Volume;

   ----------------------
   -- Design_Fuel_Mass --
   ----------------------

   function Design_Fuel_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      type Consumption_Record is
         record
            Commodity       : Harriet.Db.Commodity_Reference;
            Liquid          : Boolean;
            Cryo            : Boolean;
            Density         : Non_Negative_Real;
            Kg_Per_Second   : Non_Negative_Real;
            Relative_Mass   : Non_Negative_Real;
            Relative_Volume : Non_Negative_Real;
         end record;

      package Consumption_Record_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Consumption_Record);

      List : Consumption_Record_Lists.List;

      procedure Update_Fuel
        (Fuel          : Harriet.Db.Commodity_Reference;
         Kg_Per_Second : Non_Negative_Real);

      -----------------
      -- Update_Fuel --
      -----------------

      procedure Update_Fuel
        (Fuel          : Harriet.Db.Commodity_Reference;
         Kg_Per_Second : Non_Negative_Real)
      is
         use Harriet.Db;
         Rec : constant Harriet.Db.Commodity.Commodity_Type :=
                 Commodity.Get (Fuel);
      begin
         for Item of List loop
            if Item.Commodity = Fuel then
               Item.Kg_Per_Second :=
                 Item.Kg_Per_Second + Kg_Per_Second;
               return;
            end if;
         end loop;
         List.Append
           (Consumption_Record'
              (Commodity       => Fuel,
               Liquid          => True,
               Cryo            => True,
               Density         => Rec.Density,
               Kg_Per_Second   => Kg_Per_Second,
               Relative_Mass   => 0.0,
               Relative_Volume => 0.0));
      end Update_Fuel;

      Liquid_Tank_Size : Non_Negative_Real := 0.0;
      Cryo_Tank_Size   : Non_Negative_Real := 0.0;
      Liquid_Fuel_Mass : Non_Negative_Real := 0.0;
      Cryo_Fuel_Mass   : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db, Harriet.Db.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Drive_Component then
               declare
                  Drive : constant Drive_Component.Drive_Component_Type :=
                            Drive_Component.Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  if Drive.Fuel /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Fuel, Drive.Max_Fuel_Burn);
                  end if;

                  if Drive.Oxidiser /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Oxidiser, Drive.Max_Oxidiser_Burn);
                  end if;
               end;
            elsif Rec_Type = R_Container_Component then
               declare
                  use Harriet.Db.Container_Component;
                  Container : constant Container_Component_Type :=
                                Get_Container_Component
                                  (Module.Ship_Component);
               begin
                  if Container.Liquid then
                     if Container.Cryo then
                        Cryo_Tank_Size := Cryo_Tank_Size
                          + Harriet.Quantities.To_Real
                          (Container.Capacity);
                     else
                        Liquid_Tank_Size := Liquid_Tank_Size
                          + Harriet.Quantities.To_Real
                          (Container.Capacity);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      declare
         Total_Liquid_Mass_Consumption   : Non_Negative_Real := 0.0;
         Total_Cryo_Mass_Consumption     : Non_Negative_Real := 0.0;
         Total_Liquid_Volume_Consumption : Non_Negative_Real := 0.0;
         Total_Cryo_Volume_Consumption   : Non_Negative_Real := 0.0;
      begin
         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  Total_Cryo_Mass_Consumption :=
                    Total_Cryo_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Cryo_Volume_Consumption :=
                    Total_Cryo_Volume_Consumption
                    + Item.Kg_Per_Second / Item.Density;
               else
                  Total_Liquid_Mass_Consumption :=
                    Total_Liquid_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Liquid_Volume_Consumption :=
                    Total_Liquid_Volume_Consumption +
                      Item.Kg_Per_Second / Item.Density;
               end if;
            end if;
         end loop;

         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  declare
                     use Harriet.Real_Images;
                     function Img (X : Real) return String
                                   renames Approximate_Image;

                     This_Vol_Per_Second : constant Non_Negative_Real :=
                                             Item.Kg_Per_Second
                                               / Item.Density;
                     Partial_Volume      : constant Non_Negative_Real :=
                                             This_Vol_Per_Second
                                               / Total_Cryo_Volume_Consumption;
                     Required_Volume     : constant Non_Negative_Real :=
                                             Partial_Volume
                                               * Cryo_Tank_Size;
                     Required_Mass       : constant Non_Negative_Real :=
                                             Required_Volume
                                               * Item.Density;
                  begin
                     if False then
                        Ada.Text_IO.Put_Line
                          (Harriet.Db.Commodity.Get (Item.Commodity).Tag
                           & ": density: "
                           & Img (Item.Density)
                           & "kg/m3; consumption: "
                           & Img (Item.Kg_Per_Second)
                           & "kg/s; vol. consumption: "
                           & Img (This_Vol_Per_Second)
                           & "m3/s; partial vol: "
                           & Img (Partial_Volume)
                           & " total vol: "
                           & Img (Required_Volume)
                           & " total mass: "
                           & Img (Required_Mass));
                     end if;

                     Cryo_Fuel_Mass := Cryo_Fuel_Mass + Required_Mass;
                  end;
               else
                  Liquid_Fuel_Mass :=
                    Liquid_Fuel_Mass
                      + Item.Kg_Per_Second / Item.Density
                    * Liquid_Tank_Size;
               end if;
            end if;
         end loop;

         if False then
            Ada.Text_IO.Put_Line
              ("cryo tank size "
               & Harriet.Real_Images.Approximate_Image (Cryo_Tank_Size)
               & "; cryo fuel mass "
               & Harriet.Real_Images.Approximate_Image (Cryo_Fuel_Mass));
         end if;

         return Cryo_Fuel_Mass + Liquid_Fuel_Mass;

      end;

   end Design_Fuel_Mass;

   -----------------
   -- Design_Mass --
   -----------------

   function Design_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Mass : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db.Ship_Component;
            Component : constant Ship_Component_Type :=
                          Get (Module.Ship_Component);
         begin
            Mass := Mass + Component.Mass;
         end;
      end loop;
      return Mass;
   end Design_Mass;

   -------------------
   -- Design_Thrust --
   -------------------

   function Design_Thrust
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Thrust : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db;
            Component : constant Ship_Component.Ship_Component_Type :=
                          Ship_Component.Get (Module.Ship_Component);
         begin
            if Component.Top_Record = R_Drive_Component then
               declare
                  Drive : constant Drive_Component.Drive_Component_Type :=
                            Drive_Component.Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  Thrust := Thrust + Drive.Maximum_Thrust;
               end;
            end if;
         end;
      end loop;
      return Thrust;
   end Design_Thrust;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Mass
        (Harriet.Db.Ship.Get (Ship.Reference).Ship_Design);
   end Dry_Mass;

   ------------------
   -- Total_Thrust --
   ------------------

   function Total_Thrust
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Thrust
        (Harriet.Db.Ship.Get (Ship.Reference).Ship_Design);
   end Total_Thrust;

end Harriet.Ships;
