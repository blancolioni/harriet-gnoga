with Harriet.Managers.Installations;
with Harriet.Managers.Pops;

package body Harriet.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
      Register_Manager
        ("default-installation",
         Harriet.Managers.Installations.Create_Default_Manager'Access);
      Register_Manager
        ("hub-manager",
         Harriet.Managers.Installations.Create_Hub_Manager'Access);
      Register_Manager
        ("default-pop",
         Harriet.Managers.Pops.Create_Default_Manager'Access);
   end Register_Managers;

end Harriet.Managers.Loader;
