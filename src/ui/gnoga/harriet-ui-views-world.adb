with Harriet.Signals;

with Harriet.Elementary_Functions;

with Harriet.Factions;
with Harriet.Ships.Lists;
with Harriet.Worlds;

with Harriet.UI.Views.Model_Views;
with Harriet.UI.Views.Picture;

package body Harriet.UI.Views.World is

   package Base_View is
     new Harriet.UI.Views.Model_Views
       (Base_View_Type  =>
           Harriet.UI.Views.Picture.Root_Picture_View,
        View_Model_Type =>
           Harriet.UI.Models.World.Root_World_Model);

   type Root_World_View is
     new Base_View.View_Type with
      record
         View_Radius      : Non_Negative_Real := 1.0;
         Clock_Handler_Id : Harriet.Signals.Handler_Id;
      end record;

   type World_Access is access all Root_World_View'Class;

   overriding procedure Create
     (View    : not null access Root_World_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure On_Mouse_Click
     (View : in out Root_World_View;
      X, Y : Real);

   procedure Create_Picture
     (View : in out Root_World_View'Class);

   type World_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         World : World_Access;
      end record;

   type World_Gnoga_View_Access is
     access all World_Gnoga_View'Class;

   type World_Signal_Data is
     new Harriet.Signals.Signal_Data_Interface with
      record
         View : World_Gnoga_View_Access;
      end record;

   procedure Handle_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class);

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_World_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      use Harriet.UI.Views.Picture;

      Gnoga_View   : constant World_Gnoga_View_Access :=
                       new World_Gnoga_View;
      Picture_View : constant access Root_Picture_View :=
                       Root_Picture_View (View.all)'Access;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.World := World_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      Picture_View.Create_Picture_View (Gnoga_View.all);

      View.Set_Viewport
        (-View.View_Radius,
         -View.View_Radius,
         View.View_Radius * 2.0,
         View.View_Radius * 2.0);

      View.Create_Picture;

      declare
         Data : constant World_Signal_Data :=
                  (View => Gnoga_View);
      begin
         View.Clock_Handler_Id :=
           Session.Add_Handler
             (Signal  => Harriet.Sessions.Signal_Clock_Tick,
              Handler => Handle_Clock_Tick'Access,
              Data    => Data);
      end;

   end Create;

   --------------------
   -- Create_Picture --
   --------------------

   procedure Create_Picture
     (View : in out Root_World_View'Class)
   is

      Radius : constant Non_Negative_Real :=
                 Harriet.Worlds.Radius
                   (View.Model.World);

      Ships : Harriet.Ships.Lists.List;

   begin
      View.Clear;
      View.Font ("OpenSans", 10.0);

      Harriet.Worlds.Get_Ships (View.Model.World, Ships);

      for Ship of Ships loop
         declare
            use Harriet.Elementary_Functions;
            Orbit : constant Non_Negative_Real :=
                      Ship.Orbit / Radius;
            Longitude : constant Non_Negative_Real :=
                          Ship.Current_Longitude;
            Inclination : constant Real := Ship.Inclination;
            X           : constant Real :=
                            Orbit * Cos (Longitude, 360.0)
                            * Cos (Inclination, 360.0);
            Y           : constant Real :=
                            Orbit * Cos (Longitude, 360.0)
                            * Sin (Inclination, 360.0);
            Z           : constant Real := Orbit * Sin (Longitude, 360.0);

         begin
            if X not in -1.0 .. 1.0 or else Z > 0.0 then
               View.Fill_Color
                 (Harriet.Factions.Get (Ship.Owner).Color);
               View.Circle ((X, Y), 0.02, True);
            end if;
         end;
      end loop;

   end Create_Picture;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   procedure Handle_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      World_Signal_Data (Data).View.World.Create_Picture;
      World_Signal_Data (Data).View.World.Render;
   end Handle_Clock_Tick;

   --------------------
   -- On_Mouse_Click --
   --------------------

   overriding procedure On_Mouse_Click
     (View : in out Root_World_View;
      X, Y : Real)
   is null;

   ----------------------
   -- World_View --
   ----------------------

   function World_View
     (Model : not null access
        Harriet.UI.Models.World.Root_World_Model'Class)
      return View_Type
   is
      View : constant World_Access := new Root_World_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end World_View;

end Harriet.UI.Views.World;
