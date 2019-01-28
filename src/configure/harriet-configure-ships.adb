with Ada.Text_IO;

with Harriet.Constants;
with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Container_Component;
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
      function Get (Name : String) return Real
      is (Real (Float'(Component_Config.Get (Name, 0.0))));

      function Get
        (Child : String;
         Name  : String)
         return Real
      is (Real (Float'(Component_Config.Child (Child).Get (Name, 0.0))));

      function Get
        (Child : String;
         Index : Positive)
         return Real
      is (if Component_Config.Child (Child).Child_Count >= Index
          then Real (Float'(Component_Config.Child (Child).Get (Index)))
          else 0.0);

      function Get
        (Child : String;
         Name  : String)
         return Boolean
      is (Component_Config.Child (Child).Get (Name));

      Tag : constant String :=
              Component_Config.Config_Name;
      Mass : constant Non_Negative_Real := Get ("mass");
      Idle_Power : constant Non_Negative_Real := Get ("idle-power");
      Max_Power  : constant Non_Negative_Real := Get ("max-power");
      Linear_Accel_Limit : constant Non_Negative_Real :=
                             Get ("acceleration-limit", "linear");
      Angular_Accel_Limit : constant Non_Negative_Real :=
                              Get ("acceleration-limit", "angular");
      Normal_Temperature  : constant Non_Negative_Real :=
                              Get ("temperature-limit", "normal")
                              + Harriet.Constants.Freezing_Point_Of_Water;
      Fail_Temperature : constant Non_Negative_Real :=
                           Get ("temperature-limit", "failure")
                           + Harriet.Constants.Freezing_Point_Of_Water;
      Explosion_Power  : constant Non_Negative_Real :=
                           Get ("explosion");
      X1 : constant Real := Get ("bounding-cube", 1);
      X2 : constant Real := Get ("bounding-cube", 2);
      Y1 : constant Real := Get ("bounding-cube", 3);
      Y2 : constant Real := Get ("bounding-cube", 4);
      Z1 : constant Real := Get ("bounding-cube", 5);
      Z2 : constant Real := Get ("bounding-cube", 6);

   begin
      if Component_Config.Contains ("cargo") then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Harriet.Db.Container_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Harriet.Db.Null_Technology_Reference,
            Available           => True,
            Initial_Cost        => Harriet.Money.Zero,
            Mass                => Mass,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Capacity            =>
              Harriet.Quantities.To_Quantity
                (Get ("cargo", "volume")),
            Liquid              => Get ("cargo", "liquid"),
            Gas                 => Get ("cargo", "gas"),
            Cryo                => Get ("cargo", "cryo"));
      end if;

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
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in design '" & Design_Config.Config_Name
                  & "': no such component: "
                  & Component_Config.Config_Name);
            else
               Harriet.Db.Ship_Module_Design.Create
                 (Ship_Design    => Design,
                  Ship_Component => Component);
            end if;

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
         Directory_Name  => "ships/components",
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
         Directory_Name  => "ships/designs",
         File_Class_Name => "design",
         Process         => Configure_Design'Access);
   end Configure_Ship_Designs;

end Harriet.Configure.Ships;
