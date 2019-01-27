with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Ship_Component;
with Harriet.Db.Ship_Design;
with Harriet.Db.Ship_Module_Design;

package body Harriet.Configure.Ships is

   procedure Configure_Component
     (Component_Config : Tropos.Configuration);

   procedure Configure_Design
     (Design_Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Component_Config : Tropos.Configuration)
   is
   begin
      Harriet.Db.Ship_Component.Create
        (Tag             => Component_Config.Config_Name,
         Available       => True,
         Initial_Price   => Harriet.Money.Zero,
         Enabled_By      => Harriet.Db.Null_Technology_Reference,
         Component_Class =>
           Harriet.Db.Ship_Component_Class'Value
             (Component_Config.Get ("class")),
         Mass            => Get_Real (Component_Config, "mass"),
         Size            =>
           Component_Config.Get ("size"),
         Power           =>
           Component_Config.Get ("power", 0));
   end Configure_Component;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Design_Config : Tropos.Configuration)
   is
      Design : constant Harriet.Db.Ship_Design_Reference :=
                 Harriet.Db.Ship_Design.Create
                   (Name      => Design_Config.Config_Name,
                    Hold_Size => Harriet.Quantities.Zero);
   begin
      for Component_Config of Design_Config loop
         declare
            use Harriet.Db;
            Component : constant Harriet.Db.Ship_Component_Reference :=
                          Harriet.Db.Ship_Component.Get_Reference_By_Tag
                            (Component_Config.Config_Name);
         begin
            if Component = Null_Ship_Component_Reference then
               raise Constraint_Error with
                 "in design '" & Design_Config.Config_Name
                 & "': no such component: "
                 & Component_Config.Config_Name;
            end if;

            Harriet.Db.Ship_Module_Design.Create
              (Ship_Design    => Design,
               Ship_Component => Component);
         end;
      end loop;
   end Configure_Design;

   -------------------------------
   -- Configure_Ship_Components --
   -------------------------------

   procedure Configure_Ship_Components
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "components",
         File_Class_Name => "component",
         Process         => Configure_Component'Access);
   end Configure_Ship_Components;

   ----------------------------
   -- Configure_Ship_Designs --
   ----------------------------

   procedure Configure_Ship_Designs
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "ships",
         File_Class_Name => "design",
         Process         => Configure_Design'Access);
   end Configure_Ship_Designs;

end Harriet.Configure.Ships;
