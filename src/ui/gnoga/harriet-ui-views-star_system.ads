with Harriet.UI.Models.Star_System;

package Harriet.UI.Views.Star_System is

   function Star_System_View
     (Model : not null access
        Harriet.UI.Models.Star_System.Root_Star_System_Model'Class)
      return View_Type;

end Harriet.UI.Views.Star_System;
