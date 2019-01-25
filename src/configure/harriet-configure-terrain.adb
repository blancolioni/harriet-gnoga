with Tropos.Reader;

with Harriet.Color;

with Harriet.Db.Resource;
with Harriet.Db.Terrain;
with Harriet.Db.Terrain_Resource;

package body Harriet.Configure.Terrain is

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "terrain"),
         Extension => "terrain",
         Configure => Configure_Terrain'Access);
   end Configure_Terrain;

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is
      Color : constant Harriet.Color.Harriet_Color :=
                Harriet.Color.From_String
                  (Config.Get ("color", "#000"));
      Terrain : constant Harriet.Db.Terrain_Reference :=
                  Harriet.Db.Terrain.Create
                    (Tag      => Config.Config_Name,
                     Red      => Color.Red,
                     Green    => Color.Green,
                     Blue     => Color.Blue,
                     Hazard   => Real (Float'(Config.Get ("hazard", 0.0))),
                     Is_Water => Config.Get ("is_water"));
   begin
      if Config.Contains ("resource") then
         for Resource_Config of Config.Child ("resource").Child ("type") loop
            declare
               use Harriet.Db;
               Index    : constant String := Resource_Config.Config_Name;
               Name     : constant String := Resource_Config.Value;
               Resource : constant Resource_Reference :=
                            Harriet.Db.Resource.Get_Reference_By_Tag (Name);
               Chance   : constant Natural :=
                            Config.Child ("resource")
                            .Child ("chance")
                            .Get (Index);
            begin
               if Resource = Null_Resource_Reference then
                  raise Constraint_Error with
                    "in terrain " & Config.Config_Name
                    & ": unknown resource: " & Name;
               end if;

               Harriet.Db.Terrain_Resource.Create
                 (Terrain  => Terrain,
                  Index    => Positive'Value (Index),
                  Resource => Resource,
                  Chance   => Real (Chance) / 100.0);
            end;
         end loop;
      end if;
   end Configure_Terrain;

end Harriet.Configure.Terrain;
