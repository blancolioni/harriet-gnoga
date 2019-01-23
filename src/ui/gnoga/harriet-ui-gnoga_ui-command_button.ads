with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;

with Harriet.Commands;

package Harriet.UI.Gnoga_UI.Command_Button is

   type Command_Button is
     abstract new Gnoga.Gui.Element.Common.Button_Type with null record;

   overriding procedure Create
     (Button  : in out Command_Button;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "");

   function Command
     (Button : Command_Button)
      return Harriet.Commands.Root_Harriet_Command'Class
      is abstract;

end Harriet.UI.Gnoga_UI.Command_Button;
