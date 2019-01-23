with Harriet.Calendar;
with Harriet.Random;

with Harriet.Db.Ship;
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
                       World           => World,
                       Star_System     => World_Rec.Star_System,
                       Orbit           => World_Rec.Radius + 300.0 * 1000.0,
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

   end Create_Ship;

end Harriet.Ships;
