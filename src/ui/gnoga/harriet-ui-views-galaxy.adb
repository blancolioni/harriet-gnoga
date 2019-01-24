with Ada.Text_IO;

with Gnoga.Types;

with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.View;

with Harriet.Color;
with Harriet.Real_Images;

with Harriet.Commands;
with Harriet.Factions;
with Harriet.Star_Systems;

with Harriet.UI.Views.Model_Views;
with Harriet.UI.Views.Picture;

with Harriet.Db;

package body Harriet.UI.Views.Galaxy is

   package Base_View is
     new Harriet.UI.Views.Model_Views
       (Base_View_Type  => Harriet.UI.Views.Picture.Root_Picture_View,
        View_Model_Type => Harriet.UI.Models.Galaxy.Root_Galaxy_Model);

   type Root_Galaxy_View is
     new Base_View.View_Type with
      record
         View_Radius : Non_Negative_Real := 10.0;
         Center_X    : Real := 0.0;
         Center_Y    : Real := 0.0;
         Center_Z    : Real := 0.0;
      end record;

   overriding procedure Create
     (View    : not null access Root_Galaxy_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   overriding procedure On_Mouse_Click
     (View : in out Root_Galaxy_View;
      X, Y : Real);

   overriding procedure Draw_Picture
     (View  : in out Root_Galaxy_View;
      Layer : Harriet.UI.Views.Picture.Layer_Index);

   type Galaxy_Access is access all Root_Galaxy_View;

   type Galaxy_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Galaxy : Galaxy_Access;
      end record;

   type Galaxy_Gnoga_View_Access is access all Galaxy_Gnoga_View'Class;

   procedure Set_Fill_Color
     (Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      Color   : Harriet.Color.Harriet_Color)
     with Unreferenced;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Galaxy_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      use Harriet.UI.Views.Picture;

      Gnoga_View : constant Galaxy_Gnoga_View_Access :=
                     new Galaxy_Gnoga_View;
      Picture_View : constant access Root_Picture_View :=
                       Root_Picture_View (View.all)'Access;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Galaxy := Galaxy_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);

      Picture_View.Create_Picture_View (Gnoga_View.all);

      declare
         use Harriet.Real_Images;
         Capital_System : constant Harriet.Db.Star_System_Reference :=
                            Factions.Capital_System
                              (Session.Faction);
         Position       : constant Star_Systems.Interstellar_Position :=
                            Star_Systems.Position (Capital_System);
      begin
         View.Center_X := Position.X;
         View.Center_Y := Position.Y;
         Ada.Text_IO.Put_Line
           ("Capital: " & Star_Systems.Name (Capital_System)
            & " at ("
            & Approximate_Image (Position.X)
            & ","
            & Approximate_Image (Position.Y)
            & ","
            & Approximate_Image (Position.Z)
            & ")");
      end;

      View.Set_Viewport
        (View.Center_X - View.View_Radius,
         View.Center_Y - View.View_Radius,
         View.View_Radius * 2.0,
         View.View_Radius * 2.0);

      View.Queue_Render;

   end Create;

   ------------------
   -- Draw_Picture --
   ------------------

   overriding procedure Draw_Picture
     (View  : in out Root_Galaxy_View;
      Layer : Harriet.UI.Views.Picture.Layer_Index)
   is
      pragma Unreferenced (Layer);

      procedure Render_Star_System
        (Star_System : Harriet.Star_Systems.Star_System_Type'Class);

      -----------------
      -- Render_Star --
      -----------------

      procedure Render_Star_System
        (Star_System : Harriet.Star_Systems.Star_System_Type'Class)
      is
         Owner : constant Harriet.Factions.Faction_Type'Class :=
                   Star_System.Owner;
         Position : constant Harriet.Star_Systems.Interstellar_Position :=
                      Star_System.Position;
         Color    : constant Harriet.Color.Harriet_Color :=
                      Star_System.Primary.Color;
      begin

         if Owner.Has_Element then
            declare
               Halo_Color : Harriet.Color.Harriet_Color := Owner.Color;
            begin
               Halo_Color.Alpha := 0.6;
               View.Fill_Color (Halo_Color);
            end;
            View.Circle ((Position.X, Position.Y), 0.3, True);
         end if;

         View.Fill_Color (Color);
         View.Circle ((Position.X, Position.Y), 0.1, True);

         View.Fill_Color (Harriet.Color.White);
         View.Label ((Position.X, Position.Y), -10, 20,
                     Star_System.Name);
      end Render_Star_System;

   begin
      View.Font ("OpenSans", 12.0);
      View.Background_Color (Harriet.Color.Black);
      View.Model.Scan_Star_Systems (Render_Star_System'Access);
   end Draw_Picture;

   -----------------
   -- Galaxy_View --
   -----------------

   function Galaxy_View
     (Model : not null access Harriet.UI.Models.Galaxy.Root_Galaxy_Model'Class)
      return View_Type
   is
      View : constant Galaxy_Access := new Root_Galaxy_View;
   begin
      View.Set_Model (Model);
      return View_Type (View);
   end Galaxy_View;

   ------------------
   -- On_Key_Press --
   ------------------

--     procedure On_Key_Press
--       (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
--        Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
--     is
--        View : Galaxy_Gnoga_View renames Galaxy_Gnoga_View (Object);
--     begin
--        if Keyboard_Event.Key_Char = 'w' then
--           View.Galaxy.View_Radius :=
--             View.Galaxy.View_Radius * 0.9;
--           View.Galaxy.Render;
--        elsif Keyboard_Event.Key_Char = 'w' then
--           View.Galaxy.View_Radius :=
--             View.Galaxy.View_Radius * 1.1;
--           View.Galaxy.Render;
--        end if;
--     end On_Key_Press;

   --------------------
   -- On_Mouse_Click --
   --------------------

   overriding procedure On_Mouse_Click
     (View : in out Root_Galaxy_View;
      X, Y : Real)
   is
      use Harriet.Db;
      Star_System : constant Star_System_Reference :=
                      View.Model.Find_Star (X, Y, 0.0);
   begin
      if Star_System /= Null_Star_System_Reference then
         Harriet.Commands.Execute_Command_Line
           ("load-star-system-view --name="
            & Harriet.Star_Systems.Name (Star_System),
            View.Session,
            Harriet.Commands.Null_Writer);
      end if;
   end On_Mouse_Click;

   ---------------
   -- On_Resize --
   ---------------

--     procedure On_Resize (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
--     begin
--        Galaxy_Gnoga_View (Object).Galaxy.Resize;
--     end On_Resize;

   ------------
   -- Resize --
   ------------

--     overriding procedure Resize
--       (View : in out Root_Galaxy_View)
--     is
--     begin
--        View.Canvas.Width (View.Gnoga_View.Width - 8);
--        View.Canvas.Height (View.Gnoga_View.Height - 8);
--
--        Ada.Text_IO.Put_Line
--          ("galaxy map resize:"
--           & Natural'Image (View.Canvas.Width)
--           & " x"
--           & Natural'Image (View.Canvas.Height));
--
--        View.Render;
--     end Resize;

   --------------------
   -- Set_Fill_Color --
   --------------------

   procedure Set_Fill_Color
     (Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      Color   : Harriet.Color.Harriet_Color)
   is
      function To_RGB (X : Unit_Real)
                       return Gnoga.Types.Color_Type
      is (Gnoga.Types.Color_Type
          (X * 255.0));

      Gnoga_Color : constant Gnoga.Types.RGBA_Type := Gnoga.Types.RGBA_Type'
        (Red   => To_RGB (Color.Red),
         Green => To_RGB (Color.Green),
         Blue  => To_RGB (Color.Blue),
         Alpha => Gnoga.Types.Alpha_Type (Color.Alpha));

   begin
      Context.Fill_Color
        (Value => Gnoga_Color);
      Context.Stroke_Color
        (Value => Gnoga_Color);
   end Set_Fill_Color;

end Harriet.UI.Views.Galaxy;
