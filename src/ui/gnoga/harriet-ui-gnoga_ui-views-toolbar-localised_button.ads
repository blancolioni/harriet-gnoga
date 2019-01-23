with Ada.Strings.Unbounded;

with Gnoga.Gui.Element.Common;

with Harriet.Sessions;

private package Harriet.UI.Gnoga_UI.Views.Toolbar.Localised_Button is

   type Root_Localised_Button is
     limited new Toolbar_Item_Interface with private;

   overriding procedure Attach
     (Item   : in out Root_Localised_Button;
      View   : in out Gnoga.Gui.View.View_Type'Class);

   function Create
     (Session         : Harriet.Sessions.Harriet_Session;
      Key             : String;
      Command         : Harriet.UI.Models.Commands.Command_Type;
      Activation_Text : String := "";
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item;

private

   type Button_Item_Access is access all Root_Localised_Button'Class;

   type Localised_Gnoga_Button is
     new Gnoga.Gui.Element.Common.Button_Type with
      record
         Toolbar_Item : Button_Item_Access;
      end record;

   type Root_Localised_Button is
   limited new Toolbar_Item_Interface with
      record
         Button          : Localised_Gnoga_Button;
         Session         : Harriet.Sessions.Harriet_Session;
         Key             : Ada.Strings.Unbounded.Unbounded_String;
         Command         : Harriet.UI.Models.Commands.Command_Type;
         Activation_Text : Ada.Strings.Unbounded.Unbounded_String;
         Layout          : Toolbar_Item_Layout;
      end record;

end Harriet.UI.Gnoga_UI.Views.Toolbar.Localised_Button;
