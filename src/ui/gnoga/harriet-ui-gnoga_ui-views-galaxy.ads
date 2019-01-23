with Harriet.UI.Models.Galaxy;

package Harriet.UI.Gnoga_UI.Views.Galaxy is

   function Galaxy_View
     (Model : not null access
        Harriet.UI.Models.Galaxy.Root_Galaxy_Model'Class)
      return View_Type;

end Harriet.UI.Gnoga_UI.Views.Galaxy;
