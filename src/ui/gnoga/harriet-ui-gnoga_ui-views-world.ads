with Harriet.UI.Models.World;

package Harriet.UI.Gnoga_UI.Views.World is

   function World_View
     (Model : not null access
        Harriet.UI.Models.World.Root_World_Model'Class)
      return View_Type;

end Harriet.UI.Gnoga_UI.Views.World;
