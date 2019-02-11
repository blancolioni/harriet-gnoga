with Harriet.Db.Ship;

package Harriet.Ships is

   type Ship_Type is tagged private;

   function Get (Reference : Harriet.Db.Ship_Reference) return Ship_Type;
   function Get (Ship : Harriet.Db.Ship.Ship_Type) return Ship_Type;

   function Name (Ship : Ship_Type'Class) return String;

   function Owner
     (Ship : Ship_Type'Class)
      return Harriet.Db.Faction_Reference;

   function Star_System
     (Ship : Ship_Type'Class)
      return Harriet.Db.Star_System_Reference;

   function World
     (Ship : Ship_Type'Class)
      return Harriet.Db.World_Reference;

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Inclination
     (Ship : Ship_Type'Class)
      return Real;

   function Current_Longitude
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Total_Thrust
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Design_Cargo_Volume
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Delta_V
     (Design     : Harriet.Db.Ship_Design_Reference;
      Cargo_Mass : Non_Negative_Real)
      return Non_Negative_Real;

   function Design_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Fuel_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Thrust
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   procedure Create_Ship
     (Owner  : Harriet.Db.Faction_Reference;
      World  : Harriet.Db.World_Reference;
      Design : Harriet.Db.Ship_Design_Reference;
      Name   : String);

private

   type Ship_Type is tagged
      record
         Reference : Harriet.Db.Ship_Reference;
      end record;

   function Get (Reference : Harriet.Db.Ship_Reference) return Ship_Type
   is (Ship_Type'(Reference => Reference));

   function Get (Ship : Harriet.Db.Ship.Ship_Type) return Ship_Type
   is (Ship_Type'(Reference => Ship.Get_Ship_Reference));

   function Name (Ship : Ship_Type'Class) return String
   is (Harriet.Db.Ship.Get (Ship.Reference).Name);

   function Owner
     (Ship : Ship_Type'Class)
      return Harriet.Db.Faction_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).Faction);

   function Star_System
     (Ship : Ship_Type'Class)
      return Harriet.Db.Star_System_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).Star_System);

   function World
     (Ship : Ship_Type'Class)
      return Harriet.Db.World_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).World);

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is (Harriet.Db.Ship.Get (Ship.Reference).Orbit);

   function Inclination
     (Ship : Ship_Type'Class)
      return Real
   is (Harriet.Db.Ship.Get (Ship.Reference).Inclination);

end Harriet.Ships;
