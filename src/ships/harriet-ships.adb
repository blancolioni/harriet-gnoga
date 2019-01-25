with Ada.Numerics;
with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Random;
with Harriet.Real_Images;

with Harriet.Constants;

with Harriet.Worlds;

with Harriet.Db.Faction;
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
      Ship : constant Harriet.Db.Ship_Reference :=
                    Harriet.Db.Ship.Create
                      (Name            => Name,
                       Faction         => Owner,
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

      declare
         S : constant Ship_Type := Get (Ship);
      begin
         Ada.Text_IO.Put_Line
           (S.Name & " in orbit above "
            & Harriet.Worlds.Name (S.World)
            & ": altitude "
            & Harriet.Real_Images.Approximate_Image
              ((S.Orbit - Harriet.Worlds.Radius (S.World)) / 1_000.0)
            & "km longitude "
            & Harriet.Real_Images.Approximate_Image
              (S.Current_Longitude)
            & " degrees");
      end;
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

end Harriet.Ships;
