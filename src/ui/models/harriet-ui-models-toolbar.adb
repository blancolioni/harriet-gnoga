package body Harriet.UI.Models.Toolbar is

   function Create_Toolbar_Model
      return Toolbar_Model
   is
   begin
      return new Root_Toolbar_Model;
   end Create_Toolbar_Model;

end Harriet.UI.Models.Toolbar;
