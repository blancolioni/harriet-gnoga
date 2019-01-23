package body Harriet.UI.Views.Model_Views is

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (View : in out View_Type'Class;
      Model : not null access View_Model_Type'Class)
   is
   begin
      View.Model := Model_Access (Model);
   end Set_Model;

end Harriet.UI.Views.Model_Views;
