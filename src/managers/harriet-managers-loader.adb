with Harriet.Managers.Installations;
with Harriet.Managers.Pops;
with Harriet.Managers.Ships;

package body Harriet.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
      Register.Insert
        ("default-installation",
         Harriet.Managers.Installations.Create_Default_Manager'Access);
      Register.Insert
        ("hub-manager",
         Harriet.Managers.Installations.Create_Hub_Manager'Access);
      Register.Insert
        ("default-pop",
         Harriet.Managers.Pops.Create_Default_Manager'Access);
      Register.Insert
        ("ship-trade",
         Harriet.Managers.Ships.Create_Trade_Manager'Access);
   end Register_Managers;

end Harriet.Managers.Loader;
