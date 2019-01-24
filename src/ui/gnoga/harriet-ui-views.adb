with Ada.Unchecked_Deallocation;

package body Harriet.UI.Views is

   ------------------------
   -- Accepts_Text_Entry --
   ------------------------

   function Accepts_Text_Entry (View : Root_View_Type) return Boolean is
      pragma Unreferenced (View);
   begin
      return False;
   end Accepts_Text_Entry;

   -----------
   -- Close --
   -----------

   procedure Close
     (View : in out Root_View_Type)
   is
   begin
      View.Gnoga_View.Remove;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create_With_Gnoga_View
     (View       : in out Root_View_Type'Class;
      Session    : not null access Harriet.Sessions.Root_Harriet_Session'Class;
      Gnoga_View : not null access Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      View.Session := Harriet.Sessions.Harriet_Session (Session);
      View.Gnoga_View :=
        Gnoga.Gui.View.Pointer_To_View_Base_Class (Gnoga_View);
   end Create_With_Gnoga_View;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (View : in out View_Type)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_View_Type'Class, View_Type);
   begin
      if View /= null then
         View.Close;
         Free (View);
      end if;
   end Destroy;

end Harriet.UI.Views;
