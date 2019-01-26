with Harriet.Managers.Installations;

package body Harriet.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
      Register_Manager
        ("default-installation",
         Harriet.Managers.Installations.Create'Access);
   end Register_Managers;

end Harriet.Managers.Loader;
