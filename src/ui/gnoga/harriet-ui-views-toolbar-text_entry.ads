private with Gnoga.Gui.Element.Form;

with Harriet.Sessions;

private package Harriet.UI.Views.Toolbar.Text_Entry is

   type Root_Text_Entry is
     limited new Toolbar_Item_Interface with private;

   overriding procedure Attach
     (Item   : in out Root_Text_Entry;
      View   : in out Gnoga.Gui.View.View_Type'Class);

   function Create
     (Session         : Harriet.Sessions.Harriet_Session;
      Command         : Harriet.UI.Models.Commands.Command_Type;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

private

   type Text_Entry_Access is access all Root_Text_Entry'Class;

   type Gnoga_Text_Entry is
     new Gnoga.Gui.Element.Form.Form_Type with
      record
         Text_Entry_Item : Text_Entry_Access;
         Input           : Gnoga.Gui.Element.Form.Text_Type;
         Button          : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   type Root_Text_Entry is
   limited new Toolbar_Item_Interface with
      record
         Text_Entry      : Gnoga_Text_Entry;
         Session         : Harriet.Sessions.Harriet_Session;
         Command         : Harriet.UI.Models.Commands.Command_Type;
         Layout          : Toolbar_Item_Layout;
      end record;

end Harriet.UI.Views.Toolbar.Text_Entry;
