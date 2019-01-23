with WL.Localisation;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Table;

with Harriet.UI.Views.Model_Views;

package body Harriet.UI.Views.Tables is

   package Base_View is
     new Harriet.UI.Views.Model_Views
       (Base_View_Type  => Root_View_Type,
        View_Model_Type => Harriet.UI.Models.Tables.Root_Table_Model);

   type Root_Table_View is
     new Base_View.View_Type with
      record
         Div           : Gnoga.Gui.Element.Common.DIV_Type;
         Table         : Gnoga.Gui.Element.Table.Table_Type;
         Headings_Down : Boolean;
      end record;

   type Table_View_Access is access all Root_Table_View'Class;

   overriding procedure Create
     (View    : not null access Root_Table_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   procedure Load_Table
     (View    : in out  Root_Table_View'Class);

   type Table_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Table : Table_View_Access;
      end record;

   type Table_Gnoga_View_Access is access all Table_Gnoga_View'Class;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Table_View;
      Session : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View   : constant Table_Gnoga_View_Access :=
                       new Table_Gnoga_View;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Table := Table_View_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);
      View.Div.Create (Gnoga_View.all);
      View.Div.Class_Name ("table-view");
      View.Div.Overflow_Y (Gnoga.Gui.Element.Auto);
      View.Load_Table;
   end Create;

   -----------------------
   -- Create_Table_View --
   -----------------------

   function Create_Table_View
     (Model         : not null access
        Harriet.UI.Models.Tables.Root_Table_Model'Class;
      Headings_Down : Boolean := False)
      return View_Type
   is
      View : constant Table_View_Access := new Root_Table_View;
   begin
      View.Headings_Down := Headings_Down;
      View.Set_Model (Model);
      return View_Type (View);
   end Create_Table_View;

   ----------------
   -- Load_Table --
   ----------------

   procedure Load_Table
     (View    : in out  Root_Table_View'Class)
   is
      use Gnoga.Gui.Element.Table;
      Model : constant Harriet.UI.Models.Tables.Table_Model :=
                Harriet.UI.Models.Tables.Table_Model
                  (View.Model);
      Table : Table_Type renames View.Table;
      Header      : Table_Header_Type;
      Tbody       : Table_Body_Type;
   begin
      Table.Create (View.Div);
      Table.Class_Name ("darkTable");

      if View.Headings_Down then
         Tbody.Create (Table);

         for Column_Index in 1 .. Model.Column_Count loop
            declare
               Row : Table_Row_Type;
               Heading : Table_Heading_Type;
            begin
               Row.Create (Tbody);
               Heading.Create
                 (Row         => Row,
                  Content     =>
                    WL.Localisation.Local_Text
                      (Model.Column_Name (Column_Index)));

               for Row_Index in 1 .. Model.Row_Count loop
                  declare
                     Cell : Table_Column_Type;
                  begin
                     Cell.Create
                       (Row, Model.Image (Row_Index, Column_Index));
                  end;
               end loop;
            end;
         end loop;
      else
         declare
            Heading_Row : Table_Row_Type;
         begin
            Header.Create (Table);
            Heading_Row.Create (Header);

            for Column_Index in 1 .. Model.Column_Count loop
               declare
                  Heading : Table_Heading_Type;
               begin
                  Heading.Create
                    (Row         => Heading_Row,
                     Content     =>
                       WL.Localisation.Local_Text
                         (Model.Column_Name (Column_Index)));
               end;
            end loop;
         end;

         Tbody.Create (Table);

         for Row_Index in 1 .. Model.Row_Count loop
            declare
               Row : Table_Row_Type;
            begin
               Row.Create (Tbody);
               for Column_Index in 1 .. Model.Column_Count loop
                  declare
                     Cell : Table_Column_Type;
                  begin
                     Cell.Create
                       (Row, Model.Image (Row_Index, Column_Index));
                  end;
               end loop;
            end;
         end loop;
      end if;

   end Load_Table;

end Harriet.UI.Views.Tables;
