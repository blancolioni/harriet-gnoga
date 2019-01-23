with Harriet.UI.Models.Tables;

package Harriet.UI.Views.Tables is

   function Create_Table_View
     (Model         : not null access
        Harriet.UI.Models.Tables.Root_Table_Model'Class;
      Headings_Down : Boolean := False)
      return View_Type;

end Harriet.UI.Views.Tables;
