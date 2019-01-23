with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Gnoga.Types;

with Harriet.Real_Images;

package body Harriet.UI.Views.Picture is

   procedure On_Canvas_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Button_Down
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Button_Up
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Canvas_Mouse_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record);

   ---------
   -- Add --
   ---------

   procedure Add
     (Picture : in out Root_Picture_View'Class;
      Command : Draw_Command)
   is
   begin
      Picture.Commands.Append (Command);
   end Add;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color)
   is
   begin
      Picture.Background := Color;
   end Background_Color;

   ------------
   -- Circle --
   ------------

   procedure Circle
     (Picture : in out Root_Picture_View'Class;
      Centre  : Point_Type;
      Radius  : Non_Negative_Real;
      Filled  : Boolean)
   is
   begin
      Picture.Move_To (Centre);
      Picture.Add ((Set_Fill, Filled));
      Picture.Add ((Draw_Circle, Radius));
   end Circle;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Picture : in out Root_Picture_View'Class)
   is
   begin
      Picture.Commands.Clear;
   end Clear;

   -------------------------
   -- Create_Picture_View --
   -------------------------

   procedure Create_Picture_View
     (View       : not null access Root_Picture_View'Class;
      Gnoga_View : in out Gnoga.Gui.View.View_Type'Class)
   is
   begin
      View.Render_Canvas.View := View;
      View.Render_Canvas.Create
        (Parent => Gnoga_View,
         Width  => Gnoga_View.Width,
         Height => Gnoga_View.Height);

      View.Render_Canvas.On_Mouse_Click_Handler
        (On_Canvas_Click'Access);
      View.Render_Canvas.On_Mouse_Down_Handler
        (On_Canvas_Button_Down'Access);
      View.Render_Canvas.On_Mouse_Up_Handler
        (On_Canvas_Button_Up'Access);
      View.Render_Canvas.On_Mouse_Move_Handler
        (On_Canvas_Mouse_Move'Access);

      View.Draw_Canvas.Create
        (Parent => Gnoga_View,
         Width  => Gnoga_View.Width,
         Height => Gnoga_View.Height);
      View.Draw_Canvas.Hidden (True);

      View.Render_Context.Get_Drawing_Context_2D (View.Render_Canvas);
      View.Draw_Context.Get_Drawing_Context_2D (View.Draw_Canvas);

   end Create_Picture_View;

   ----------------
   -- Draw_Color --
   ----------------

   procedure Draw_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color)
   is
   begin
      Picture.Add ((Set_Draw_Color, Color));
   end Draw_Color;

   ----------------
   -- Fill_Color --
   ----------------

   procedure Fill_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color)
   is
   begin
      Picture.Add ((Set_Fill_Color, Color));
   end Fill_Color;

   ----------
   -- Font --
   ----------

   procedure Font
     (Picture : in out Root_Picture_View'Class;
      Family  : String;
      Size    : Non_Negative_Real;
      Bold    : Boolean := False;
      Italic  : Boolean := False)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Add
        ((Set_Font, To_Unbounded_String (Family), Size, Bold, Italic));
   end Font;

   -----------
   -- Label --
   -----------

   procedure Label
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Offset_X : Integer;
      Offset_Y : Integer;
      Text     : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Move_To (Position);
      Picture.Add ((Draw_Text, Offset_X, Offset_Y,
                   To_Unbounded_String (Text)));
   end Label;

   ----------
   -- Line --
   ----------

   procedure Line
     (Picture  : in out Root_Picture_View'Class;
      From, To : Point_Type)
   is
   begin
      Picture.Add ((Draw_Line, From, To));
   end Line;

   -------------
   -- Line_To --
   -------------

   procedure Line_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type)
   is
   begin
      Picture.Add ((Draw_Line_To, False, Position));
   end Line_To;

   ----------------
   -- Line_Width --
   ----------------

   procedure Line_Width
     (Picture : in out Root_Picture_View'Class;
      Width   : Non_Negative_Real)
   is
   begin
      Picture.Add ((Set_Line_Width, Width));
   end Line_Width;

   -------------------
   -- Move_Relative --
   -------------------

   procedure Move_Relative
     (Picture : in out Root_Picture_View'Class;
      Offset  : Point_Type)
   is
   begin
      Picture.Add ((Move_To, True, Offset));
   end Move_Relative;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type)
   is
   begin
      Picture.Add ((Move_To, False, Position));
   end Move_To;

   ---------------------------
   -- On_Canvas_Button_Down --
   ---------------------------

   procedure On_Canvas_Button_Down
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      View.Dragging := True;
      View.Previous_X := Event.X;
      View.Previous_Y := Event.Y;
   end On_Canvas_Button_Down;

   -------------------------
   -- On_Canvas_Button_Up --
   -------------------------

   procedure On_Canvas_Button_Up
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      pragma Unreferenced (Event);
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      View.Dragging := False;
   end On_Canvas_Button_Up;

   ---------------------
   -- On_Canvas_Click --
   ---------------------

   procedure On_Canvas_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;

      View_Size : constant Non_Negative_Real :=
                    Real'Min (View.Viewport.Width, View.Viewport.Height);
      Screen_Width   : constant Natural := View.Render_Canvas.Width;
      Screen_Height  : constant Natural := View.Render_Canvas.Height;

      To_View_Scale  : constant Non_Negative_Real :=
                         View_Size
                           / Real (Natural'Min (Screen_Width, Screen_Height));

      Center_X        : constant Real :=
                   View.Viewport.Left_Top.X
                     + View.Viewport.Width / 2.0;
      Center_Y : constant Real :=
                   View.Viewport.Left_Top.Y
                     + View.Viewport.Height / 2.0;

      function To_View_X (X : Integer) return Real
      is (Real (X - Screen_Width / 2) * To_View_Scale
          + Center_X);

      function To_View_Y (Y : Integer) return Real
      is (Real (Y - Screen_Height / 2) * To_View_Scale
          + Center_Y);

      X : constant Real := To_View_X (Event.X);
      Y : constant Real := To_View_Y (Event.Y);

   begin
      Ada.Text_IO.Put_Line
        ("canvas click" & Event.X'Image & Event.Y'Image
         & " -> "
         & Harriet.Real_Images.Approximate_Image (X)
         & " "
         & Harriet.Real_Images.Approximate_Image (Y));
      View.On_Mouse_Click (X, Y);

   end On_Canvas_Click;

   --------------------------
   -- On_Canvas_Mouse_Move --
   --------------------------

   procedure On_Canvas_Mouse_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : Gnoga.Gui.Base.Mouse_Event_Record)
   is
      Canvas : Picture_Canvas renames Picture_Canvas (Object);
      View   : constant access Root_Picture_View'Class := Canvas.View;
   begin
      if View.Dragging then
         declare
            DY : constant Integer := Event.Y - View.Previous_Y;
         begin
            View.X_Axis_Rotation := View.X_Axis_Rotation + Real (DY);
         end;
      end if;
   end On_Canvas_Mouse_Move;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real;
      Filled   : Boolean)
   is
   begin
      Picture.Add ((Set_Fill, Filled));
      Picture.Move_To (Position);
      Picture.Add ((Draw_Rectangle, Width, Height));
   end Rectangle;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (View : in out Root_Picture_View)
   is
      View_Size : constant Non_Negative_Real :=
                    Real'Min (View.Viewport.Width, View.Viewport.Height);
      Screen_Width   : constant Natural := View.Render_Canvas.Width;
      Screen_Height  : constant Natural := View.Render_Canvas.Height;

      To_Screen_Scale : constant Non_Negative_Real :=
                          Real (Natural'Min (Screen_Width, Screen_Height))
                          / View_Size;

      Center_X : constant Real :=
                   View.Viewport.Left_Top.X
                     + View.Viewport.Width / 2.0;
      Center_Y : constant Real :=
                   View.Viewport.Left_Top.Y
                     + View.Viewport.Height / 2.0;

      function To_Screen_Distance
        (Distance : Non_Negative_Real)
         return Natural
      is (Natural (Distance * To_Screen_Scale));

      function To_Screen_X (X : Real) return Integer
      is (Integer ((X - Center_X) * To_Screen_Scale));

      function To_Screen_Y (Y : Real) return Integer
      is (Integer ((Y - Center_Y) * To_Screen_Scale));

      Current_Position         : Point_Type := (0.0, 0.0);
      Current_Fill             : Boolean := False;
      Current_Fill_Color       : Harriet.Color.Harriet_Color;
      Current_Draw_Color       : Harriet.Color.Harriet_Color;
      Fill_Color_Changed       : Boolean := False;
      Draw_Color_Changed       : Boolean := False;

      Current_Font_Family      : Ada.Strings.Unbounded.Unbounded_String;
      Current_Font_Size        : Non_Negative_Real;
      Current_Font_Changed     : Boolean := False;

      Current_Line_Width       : Non_Negative_Real := 1.0;
      Line_Width_Changed       : Boolean := False;

      Drawing_Line             : Boolean := False;

      Count : Natural := 0;

      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type renames
                  View.Draw_Context;

      function Is_Visible (P : Point_Type) return Boolean
      is (Contains (View.Viewport, P));

      procedure Check_Path;

      procedure Execute (Command : Draw_Command);

      ----------------
      -- Check_Path --
      ----------------

      procedure Check_Path is
      begin
         if Drawing_Line then
            Drawing_Line := False;
            Context.Stroke;
         end if;
      end Check_Path;

      -------------
      -- Execute --
      -------------

      procedure Execute (Command : Draw_Command) is
         use Ada.Strings.Unbounded;
         use Harriet.Color;
         use Harriet.Real_Images;

         procedure Check_Colors (Fill : Boolean);
         procedure Check_Lines;

         ------------------
         -- Check_Colors --
         ------------------

         procedure Check_Colors (Fill : Boolean) is
         begin
            if Fill then
               if Fill_Color_Changed then
                  Context.Fill_Color
                    (Harriet.Color.To_Html_String (Current_Fill_Color));
                  Fill_Color_Changed := False;
               end if;
            else
               if Draw_Color_Changed then
                  Check_Path;
                  Context.Stroke_Color
                    (Harriet.Color.To_Html_String (Current_Draw_Color));
                  Draw_Color_Changed := False;
               end if;
            end if;

         end Check_Colors;

         -----------------
         -- Check_Lines --
         -----------------

         procedure Check_Lines is
         begin
            if Line_Width_Changed then
               Context.Line_Width (Natural (Current_Line_Width));
               Line_Width_Changed := False;
            end if;
         end Check_Lines;

      begin
         case Command.Primitive is
            when Set_Draw_Color =>
               Current_Draw_Color := Command.Color;
               Draw_Color_Changed := True;
            when Set_Fill_Color =>
               if Command.Color /= Current_Fill_Color then
                  Current_Fill_Color := Command.Color;
                  Fill_Color_Changed := True;
               end if;
            when Set_Font =>
               if Command.Font_Family /= Current_Font_Family then
                  Current_Font_Family := Command.Font_Family;
                  Current_Font_Changed := True;
               end if;
               if Command.Font_Size /= Current_Font_Size then
                  Current_Font_Size := Command.Font_Size;
                  Current_Font_Changed := True;
               end if;
            when Set_Line_Width =>
               if Command.Line_Width /= Current_Line_Width then
                  Current_Line_Width := Command.Line_Width;
                  Line_Width_Changed := True;
               end if;

            when Draw_Line_To =>
               Check_Colors (False);
               Check_Lines;

               Context.Move_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               if not Drawing_Line then
                  Context.Begin_Path;
               end if;

               Current_Position := Command.Position;

               Context.Line_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               Drawing_Line := True;

            when Draw_Line =>
               Check_Colors (False);
               Check_Lines;

               Current_Position := Command.Line_From;
               Context.Move_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               if not Drawing_Line then
                  Context.Begin_Path;
               end if;

               Current_Position := Command.Line_To;

               Context.Line_To
                 (To_Screen_X (Current_Position.X),
                  To_Screen_Y (Current_Position.Y));

               Drawing_Line := True;

            when Move_To =>
               if Command.Relative then
                  Current_Position.X :=
                    Current_Position.X + Command.Position.X;
                  Current_Position.Y :=
                    Current_Position.Y + Command.Position.Y;
               else
                  Current_Position := Command.Position;
               end if;

            when Set_Fill =>
               Current_Fill := Command.Fill;

            when Draw_Circle =>
               if Is_Visible (Current_Position) then
                  Check_Path;
                  Check_Lines;
                  Check_Colors (Current_Fill);
                  Context.Begin_Path;
                  Context.Arc_Degrees
                    (X                 => To_Screen_X (Current_Position.X),
                     Y                 => To_Screen_Y (Current_Position.Y),
                     Radius            => To_Screen_Distance (Command.Radius),
                     Starting_Angle    => 0.0,
                     Ending_Angle      => 360.0);
                  Context.Close_Path;
                  if Current_Fill then
                     Context.Fill;
                  else
                     Context.Stroke;
                  end if;
                  Count := Count + 1;
               end if;

            when Draw_Rectangle =>

               if Is_Visible (Current_Position)
                 or else Is_Visible ((Current_Position.X + Command.Width,
                                     Current_Position.Y + Command.Height))
               then
                  Check_Path;
                  Check_Lines;
                  Check_Colors (Current_Fill);
                  Context.Rectangle
                    (Rectangle => Gnoga.Types.Rectangle_Type'
                       (X      => To_Screen_X (Current_Position.X),
                        Y      => To_Screen_Y (Current_Position.Y),
                        Width  => To_Screen_Distance (Command.Width),
                        Height => To_Screen_Distance (Command.Height)));
                  if Current_Fill then
                     Context.Fill;
                  else
                     Context.Stroke;
                  end if;
                  Count := Count + 1;
               end if;

            when Draw_Text =>
               if Is_Visible (Current_Position) then
                  if Current_Font_Changed then
                     Context.Font
                       (Family  => To_String (Current_Font_Family),
                        Height  =>
                          Approximate_Image (Current_Font_Size) & "pt");
                     Current_Font_Changed := False;
                  end if;
                  Check_Path;
                  Check_Lines;
                  Check_Colors (True);
                  Context.Fill_Text
                    (Text       => To_String (Command.Text),
                     X          =>
                       Command.Offset_X + To_Screen_X (Current_Position.X),
                     Y          =>
                       Command.Offset_Y + To_Screen_Y (Current_Position.Y));
                  Count := Count + 1;
               end if;
         end case;
      end Execute;

      Start : constant Ada.Calendar.Time :=
                Ada.Calendar.Clock;

   begin
--        Ada.Text_IO.Put_Line
--          ("picture render: width"
--           & Natural'Image (View.Gnoga_View.Width)
--           & " height"
--           & Natural'Image (View.Gnoga_View.Height)
--           & " viewport "
--           & Harriet.Real_Images.Approximate_Image (View.Viewport.Left_Top.X)
--           & " "
--           & Harriet.Real_Images.Approximate_Image (View.Viewport.Left_Top.Y)
--           & " "
--           & Harriet.Real_Images.Approximate_Image (View.Viewport.Width)
--           & "x"
--           & Harriet.Real_Images.Approximate_Image (View.Viewport.Height));

      Context.Fill_Color
        (Harriet.Color.To_Html_String (View.Background));

      Context.Fill_Rectangle
        (Gnoga.Types.Rectangle_Type'
           (X      => 0,
            Y      => 0,
            Width  => Screen_Width,
            Height => Screen_Height));

      Context.Save;

      Context.Translate (Screen_Width / 2, Screen_Height / 2);

      for Command of View.Commands loop
         Execute (Command);
      end loop;

      Context.Restore;

      View.Render_Context.Draw_Image
        (View.Draw_Canvas, 0, 0);

      declare
         use type Ada.Calendar.Time;
         Finish : constant Ada.Calendar.Time :=
                    Ada.Calendar.Clock;
      begin
         if False then
            Ada.Text_IO.Put_Line
              ("executed"
               & Count'Image
               & " primitives in "
               & Harriet.Real_Images.Approximate_Image
                 (Real (Finish - Start))
               & "s");
         end if;
      end;

   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (View : in out Root_Picture_View)
   is
      function Image (X : Natural) return String
      is (Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Left));

   begin
      View.Render_Canvas.Attribute ("width", Image (View.Gnoga_View.Width));
      View.Render_Canvas.Attribute ("height", Image (View.Gnoga_View.Height));
      View.Draw_Canvas.Attribute ("width", Image (View.Gnoga_View.Width));
      View.Draw_Canvas.Attribute ("height", Image (View.Gnoga_View.Height));
      View.Render;
   end Resize;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport
     (Picture  : in out Root_Picture_View'Class;
      Left     : Real;
      Top      : Real;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real)
   is
   begin
      Picture.Viewport := ((Left, Top), Width, Height);
   end Set_Viewport;

   ----------
   -- Text --
   ----------

   procedure Text
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Text     : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Picture.Move_To (Position);
      Picture.Add ((Draw_Text, 0, 0, To_Unbounded_String (Text)));
   end Text;

end Harriet.UI.Views.Picture;
