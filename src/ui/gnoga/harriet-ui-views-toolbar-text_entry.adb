with WL.Localisation;

package body Harriet.UI.Views.Toolbar.Text_Entry is

   procedure On_Form_Submit
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Item   : in out Root_Text_Entry;
      View   : in out Gnoga.Gui.View.View_Type'Class)
   is
   begin
      Item.Text_Entry.Create (View);
      Item.Text_Entry.Input.Create (Item.Text_Entry);
      Item.Text_Entry.Button.Create
        (Item.Text_Entry,  WL.Localisation.Local_Text ("execute-command"));
      Item.Text_Entry.On_Submit_Handler (On_Form_Submit'Access);
   end Attach;

   ------------
   -- Create --
   ------------

   function Create
     (Session         : Harriet.Sessions.Harriet_Session;
      Command         : Harriet.UI.Models.Commands.Command_Type;
      Layout          : Toolbar_Item_Layout := Default_Layout)
      return Toolbar_Item
   is
      Item : constant Text_Entry_Access := new Root_Text_Entry'
        (Text_Entry => <>,
         Session    => Session,
         Command    => Command,
         Layout     => Layout);
   begin
      Item.Text_Entry.Text_Entry_Item := Item;
      return Toolbar_Item (Item);
   end Create;

   --------------------
   -- On_Form_Submit --
   --------------------

   procedure On_Form_Submit
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Gnoga_Item : Gnoga_Text_Entry renames Gnoga_Text_Entry (Object);
   begin
      if Gnoga_Item.Input.Value /= "" then
         Gnoga_Item.Text_Entry_Item.Command.On_Activated
           (Session => Gnoga_Item.Text_Entry_Item.Session,
            Value   => Gnoga_Item.Input.Value);
         Gnoga_Item.Input.Value ("");
      end if;
   end On_Form_Submit;

end Harriet.UI.Views.Toolbar.Text_Entry;
