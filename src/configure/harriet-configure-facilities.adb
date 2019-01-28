with Tropos.Reader;

with Harriet.Quantities;

with Harriet.Db.Fissile;
with Harriet.Db.Fuel;
with Harriet.Db.Gas;
with Harriet.Db.Liquid;
with Harriet.Db.Metal;
with Harriet.Db.Mineral;
with Harriet.Db.Organic;

with Harriet.Db.Facility;
with Harriet.Db.Facility_Module;
with Harriet.Db.Facility_Worker;
with Harriet.Db.Pop_Group;

with Harriet.Db.Building_Module;
with Harriet.Db.Consumer_Good;
with Harriet.Db.Industrial_Good;
with Harriet.Db.Ship_Component;

with Harriet.Db.Colony_Hub;
with Harriet.Db.Resource_Generator;
with Harriet.Db.Factory;
with Harriet.Db.Educational;
with Harriet.Db.Entertainment;
with Harriet.Db.Fitness;
with Harriet.Db.Medical;

with Harriet.Db.Generated_Resource;
with Harriet.Db.Produced_Commodity;

package body Harriet.Configure.Facilities is

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   procedure Configure_Employees
     (Config : Tropos.Configuration);

   procedure Configure_Modules
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Employees --
   -------------------------

   procedure Configure_Employees
     (Config : Tropos.Configuration)
   is
      Facility : constant Harriet.Db.Facility_Reference :=
                   Harriet.Db.Facility.Get_Reference_By_Tag
                     (Config.Config_Name);
      Emp_Config : constant Tropos.Configuration :=
                     Config.Child ("worker");
      Emp_Type   : constant Tropos.Configuration :=
                     Emp_Config.Child ("type");
      Emp_Qty    : constant Tropos.Configuration :=
                     Emp_Config.Child ("qty");
   begin
      for Type_Config of Emp_Type loop
         declare
            Group : constant Harriet.Db.Pop_Group_Reference :=
                      Harriet.Db.Pop_Group.Get_Reference_By_Tag
                        (Type_Config.Value);
            Quantity : constant Harriet.Quantities.Quantity_Type :=
                         Harriet.Quantities.To_Quantity
                           (Real
                              (Float'(Emp_Qty.Get
                               (Type_Config.Config_Name))));
         begin
            Harriet.Db.Facility_Worker.Create
              (Facility  => Facility,
               Pop_Group => Group,
               Quantity  => Quantity);
         end;
      end loop;
   end Configure_Employees;

   ---------------------------
   -- Configure_Facilities --
   ---------------------------

   procedure Configure_Facilities
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "facilities"),
         Extension => "facility",
         Configure => Configure_Facility'Access);
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "facilities"),
         Extension => "facility",
         Configure => Configure_Employees'Access);
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "facilities"),
         Extension => "facility",
         Configure => Configure_Modules'Access);
   end Configure_Facilities;

   -------------------------
   -- Configure_Facility --
   -------------------------

   procedure Configure_Facility
     (Config : Tropos.Configuration)
   is
      Tag           : constant String := Config.Config_Name;
      Class         : constant String := Config.Get ("class");
      Quality       : constant Natural := Config.Get ("quality", 0);
      Power         : constant Real :=
                        Real (Float'(Config.Get ("power", 0.0)));
      Educational   : constant Boolean := Config.Get ("educational");
      Entertainment : constant Boolean := Config.Get ("entertainment");
      Fitness       : constant Boolean := Config.Get ("fitness");
      Medical       : constant Boolean := Config.Get ("medical");

      Construction  : constant Boolean := Config.Get ("construction");
      Consumer      : constant Boolean := Config.Get ("consumer");
      Industrial    : constant Boolean := Config.Get ("industrial");
      Shipyard      : constant Boolean := Config.Get ("shipyard");

      Fissile       : constant Boolean := Config.Get ("fissile");
      Fuel          : constant Boolean := Config.Get ("fuel");
      Gas           : constant Boolean := Config.Get ("gas");
      Liquid        : constant Boolean := Config.Get ("liquid");
      Metal         : constant Boolean := Config.Get ("metal");
      Mineral       : constant Boolean := Config.Get ("mineral");
      Organic       : constant Boolean := Config.Get ("organic");

      Capacity      : constant Harriet.Quantities.Quantity_Type :=
                        Harriet.Quantities.To_Quantity
                          (Real (Float'(Config.Get ("capacity", 0.0))));

   begin

      if Class = "factory" then

         declare
            Factory : constant Harriet.Db.Factory_Reference :=
                        Harriet.Db.Factory.Create
                          (Power    => Power,
                           Capacity => Capacity,
                           Tag      => Tag);
         begin
            if Construction then
               for Commodity of Harriet.Db.Building_Module.Scan_By_Tag loop
                  Harriet.Db.Produced_Commodity.Create
                    (Factory   => Factory,
                     Commodity => Commodity.Reference);
               end loop;
            end if;

            if Consumer then
               for Commodity of Harriet.Db.Consumer_Good.Scan_By_Tag loop
                  Harriet.Db.Produced_Commodity.Create
                    (Factory   => Factory,
                     Commodity => Commodity.Reference);
               end loop;
            end if;

            if Industrial then
               for Commodity of Harriet.Db.Industrial_Good.Scan_By_Tag loop
                  Harriet.Db.Produced_Commodity.Create
                    (Factory   => Factory,
                     Commodity => Commodity.Reference);
               end loop;
            end if;

            if Shipyard then
               for Commodity of Harriet.Db.Ship_Component.Scan_By_Tag loop
                  Harriet.Db.Produced_Commodity.Create
                    (Factory   => Factory,
                     Commodity => Commodity.Reference);
               end loop;
            end if;

         end;
      elsif Class = "resource-generator" then
         declare
            Generator : constant Harriet.Db.Resource_Generator_Reference :=
                          Harriet.Db.Resource_Generator.Create
                            (Power    => Power,
                             Capacity => Capacity,
                             Tag      => Tag);
         begin
            if Fissile then
               for Fissile of Harriet.Db.Fissile.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Fissile.Reference);
               end loop;
            end if;
            if Fuel then
               for Fuel of Harriet.Db.Fuel.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Fuel.Reference);
               end loop;
            end if;
            if Gas then
               for Gas of Harriet.Db.Gas.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Gas.Reference);
               end loop;
            end if;
            if Liquid then
               for Liquid of Harriet.Db.Liquid.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Liquid.Reference);
               end loop;
            end if;
            if Metal then
               for Metal of Harriet.Db.Metal.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Metal.Reference);
               end loop;
            end if;
            if Mineral then
               for Mineral of Harriet.Db.Mineral.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Mineral.Reference);
               end loop;
            end if;
            if Organic then
               for Organic of Harriet.Db.Organic.Scan_By_Tag loop
                  Harriet.Db.Generated_Resource.Create
                    (Resource_Generator => Generator,
                     Resource           => Organic.Reference);
               end loop;
            end if;
         end;
      elsif Class = "service-facility" then
         if Educational then
            Harriet.Db.Educational.Create
              (Quality  => Quality,
               Capacity => Capacity,
               Power    => Power,
               Tag      => Tag);
         elsif Entertainment then
            Harriet.Db.Entertainment.Create
              (Quality  => Quality,
               Capacity => Capacity,
               Power    => Power,
               Tag      => Tag);
         elsif Fitness then
            Harriet.Db.Fitness.Create
              (Quality  => Quality,
               Capacity => Capacity,
               Power    => Power,
               Tag      => Tag);
         elsif Medical then
            Harriet.Db.Medical.Create
              (Quality  => Quality,
               Capacity => Capacity,
               Power    => Power,
               Tag      => Tag);
         end if;
      elsif Class = "hub" then
         Harriet.Db.Colony_Hub.Create
           (Capacity => Capacity,
            Power    => Power,
            Tag      => Tag);
      end if;
   end Configure_Facility;

   -----------------------
   -- Configure_Modules --
   -----------------------

   procedure Configure_Modules
     (Config : Tropos.Configuration)
   is
      Facility   : constant Harriet.Db.Facility_Reference :=
                     Harriet.Db.Facility.Get_Reference_By_Tag
                       (Config.Config_Name);
      Mod_Config : constant Tropos.Configuration :=
                     Config.Child ("module");
      Mod_Type   : constant Tropos.Configuration :=
                     Mod_Config.Child ("type");
      Mod_Qty    : constant Tropos.Configuration :=
                     Mod_Config.Child ("qty");
   begin
      for Type_Config of Mod_Type loop
         declare
            Module    : constant Harriet.Db.Building_Module_Reference :=
                         Harriet.Db.Building_Module.Get_Reference_By_Tag
                           (Type_Config.Value);
            Quantity : constant Harriet.Quantities.Quantity_Type :=
                         Harriet.Quantities.To_Quantity
                           (Real
                              (Float'(Mod_Qty.Get
                               (Type_Config.Config_Name))));
         begin
            Harriet.Db.Facility_Module.Create
              (Facility        => Facility,
               Building_Module => Module,
               Quantity        => Quantity);
         end;
      end loop;
   end Configure_Modules;

end Harriet.Configure.Facilities;
