private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with Gnoga.Gui.Element.Canvas.Context_2D;

with Harriet.Color;

package Harriet.UI.Gnoga_UI.Views.Picture is

   type Point_Type is
      record
         X, Y : Real;
      end record;

   type Rectangle_Type is
      record
         Left_Top : Point_Type;
         Width    : Non_Negative_Real;
         Height   : Non_Negative_Real;
      end record;

   function Contains
     (R : Rectangle_Type;
      P : Point_Type)
      return Boolean;

   type Root_Picture_View is abstract new Root_View_Type with private;

   procedure Create_Picture_View
     (View       : not null access Root_Picture_View'Class;
      Gnoga_View : in out Gnoga.Gui.View.View_Type'Class);

   overriding procedure Render
     (View : in out Root_Picture_View);

   overriding procedure Resize
     (View : in out Root_Picture_View);

   procedure Set_Viewport
     (Picture  : in out Root_Picture_View'Class;
      Left     : Real;
      Top      : Real;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real);

   procedure On_Mouse_Click
     (Picture : in out Root_Picture_View;
      X, Y    : Real)
   is null;

   procedure Clear
     (Picture : in out Root_Picture_View'Class);

   procedure Draw_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color);

   procedure Fill_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color);

   procedure Background_Color
     (Picture : in out Root_Picture_View'Class;
      Color   : Harriet.Color.Harriet_Color);

   procedure Move_To
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type);

   procedure Move_Relative
     (Picture : in out Root_Picture_View'Class;
      Offset  : Point_Type);

   procedure Font
     (Picture : in out Root_Picture_View'Class;
      Family  : String;
      Size    : Non_Negative_Real;
      Bold    : Boolean := False;
      Italic  : Boolean := False);

   procedure Circle
     (Picture : in out Root_Picture_View'Class;
      Centre  : Point_Type;
      Radius  : Non_Negative_Real;
      Filled  : Boolean);

   procedure Rectangle
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Width    : Non_Negative_Real;
      Height   : Non_Negative_Real;
      Filled   : Boolean);

   procedure Text
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Text     : String);

   procedure Label
     (Picture  : in out Root_Picture_View'Class;
      Position : Point_Type;
      Offset_X : Integer;
      Offset_Y : Integer;
      Text     : String);

private

   type Draw_Primitive is
     (Set_Draw_Color, Set_Fill_Color, Set_Font,
      Move_To, Set_Fill,
      Draw_Circle, Draw_Rectangle, Draw_Text);

   type Draw_Command (Primitive : Draw_Primitive) is
      record
         case Primitive is
            when Set_Draw_Color | Set_Fill_Color =>
               Color : Harriet.Color.Harriet_Color;
            when Set_Font =>
               Font_Family : Ada.Strings.Unbounded.Unbounded_String;
               Font_Size   : Non_Negative_Real;
               Font_Bold   : Boolean;
               Font_Italic : Boolean;
            when Move_To =>
               Relative    : Boolean;
               Position    : Point_Type;
            when Set_Fill =>
               Fill        : Boolean;
            when Draw_Circle =>
               Radius      : Non_Negative_Real;
            when Draw_Rectangle =>
               Width       : Non_Negative_Real;
               Height      : Non_Negative_Real;
            when Draw_Text =>
               Offset_X    : Integer;
               Offset_Y    : Integer;
               Text        : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Draw_Command_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Draw_Command);

   type Picture_Canvas is
     new Gnoga.Gui.Element.Canvas.Canvas_Type with
      record
         View : access Root_Picture_View'Class;
      end record;

   type Root_Picture_View is abstract new Root_View_Type with
      record
         Background      : Harriet.Color.Harriet_Color := Harriet.Color.Black;
         Commands        : Draw_Command_Lists.List;
         Render_Canvas   : Picture_Canvas;
         Draw_Canvas     : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Render_Context  : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
         Draw_Context    : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
         Viewport        : Rectangle_Type := ((0.0, 0.0), 1.0, 1.0);
         X_Axis_Rotation : Real := 45.0;
         Dragging        : Boolean := False;
         Previous_X      : Integer;
         Previous_Y      : Integer;
      end record;

   procedure Add
     (Picture : in out Root_Picture_View'Class;
      Command : Draw_Command);

   function Contains
     (R : Rectangle_Type;
      P : Point_Type)
      return Boolean
   is (P.X in R.Left_Top.X .. R.Left_Top.X + R.Width
       and then P.Y in R.Left_Top.Y .. R.Left_Top.Y + R.Height);

end Harriet.UI.Gnoga_UI.Views.Picture;
