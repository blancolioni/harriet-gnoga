with Harriet.UI.Models.Dashboard;

package Harriet.UI.Views.Dashboard is

   function Dashboard_View
     (Model : not null access
        Harriet.UI.Models.Dashboard.Root_Dashboard_Model'Class)
      return View_Type;

end Harriet.UI.Views.Dashboard;
