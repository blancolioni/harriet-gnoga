with Harriet.UI.Models.Login;

package Harriet.UI.Views.Login is

   function Login_View
     (Model : not null access Harriet.UI.Models.Login.Root_Login_Model'Class)
      return View_Type;

end Harriet.UI.Views.Login;
