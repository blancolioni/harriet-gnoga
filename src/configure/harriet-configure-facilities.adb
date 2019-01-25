with Tropos.Reader;

with Harriet.Db.Fissile;
with Harriet.Db.Fuel;
with Harriet.Db.Gas;
with Harriet.Db.Liquid;
with Harriet.Db.Metal;
with Harriet.Db.Mineral;
with Harriet.Db.Organic;

with Harriet.Db.Building_Module;
with Harriet.Db.Consumer_Good;
with Harriet.Db.Industrial_Good;
with Harriet.Db.Ship_Component;

with Harriet.Db.Resource_Generator;
with Harriet.Db.Factory;
with Harriet.Db.Educational_Facility;
with Harriet.Db.Entertainment_Facility;
with Harriet.Db.Fitness_Facility;
with Harriet.Db.Medical_Facility;

with Harriet.Db.Generated_Resource;
with Harriet.Db.Produced_Commodity;

package body Harriet.Configure.Facilities is

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   ---------------------------
   -- Configure_Facilities --
   ---------------------------

   procedure Configure_Facilities
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "Facilities"),
         Extension => "Facility",
         Configure => Configure_Facility'Access);
   end Configure_Facilities;

   -------------------------
   -- Configure_Facility --
   -------------------------

   procedure Configure_Facility
     (Config : Tropos.Configuration)
   is
      Tag         : constant String := Config.Config_Name;
      Class       : constant String := Config.Get ("class");
      Quality     : constant Natural := Config.Get ("quality", 0);
      Power       : constant Real :=
                      Real (Float'(Config.Get ("power", 0.0)));
      Educational   : constant Boolean := Config.Get ("educational");
      Entertainment : constant Boolean := Config.Get ("entertainment");
      Fitness       : constant Boolean := Config.Get ("fitness");
      Medical       : constant Boolean := Config.Get ("medical");

      Construction  : constant Boolean := Config.Get ("construction");
      Consumer      : constant Boolean := Config.Get ("consumer");
      Industrial    : constant Boolean := Config.Get ("industrial");
      Shipyard      : constant Boolean := Config.Get ("shipyard");

      Fissile     : constant Boolean := Config.Get ("fissile");
      Fuel        : constant Boolean := Config.Get ("fuel");
      Gas         : constant Boolean := Config.Get ("gas");
      Liquid      : constant Boolean := Config.Get ("liquid");
      Metal       : constant Boolean := Config.Get ("metal");
      Mineral     : constant Boolean := Config.Get ("mineral");
      Organic     : constant Boolean := Config.Get ("organic");

   begin

      if Class = "factory" then

         declare
            Factory : constant Harriet.Db.Factory_Reference :=
                        Harriet.Db.Factory.Create
                          (Power => Power,
                           Tag   => Tag);
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
                            (Power => Power,
                             Tag   => Tag);
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
            Harriet.Db.Educational_Facility.Create
              (Quality => Quality,
               Power   => Power,
               Tag     => Tag);
         elsif Entertainment then
            Harriet.Db.Entertainment_Facility.Create
              (Quality => Quality,
               Power   => Power,
               Tag     => Tag);
         elsif Fitness then
            Harriet.Db.Fitness_Facility.Create
              (Quality => Quality,
               Power   => Power,
               Tag     => Tag);
         elsif Medical then
            Harriet.Db.Medical_Facility.Create
              (Quality => Quality,
               Power   => Power,
               Tag     => Tag);
         end if;
      end if;
   end Configure_Facility;

end Harriet.Configure.Facilities;
