with Harriet.Commands.System;
with Harriet.Commands.Views;

package body Harriet.Commands.Loader is

   -------------------
   -- Load_Commands --
   -------------------

   procedure Load_Commands is
   begin
      Harriet.Commands.System.Load_System_Commands;
      Harriet.Commands.Views.Load_View_Commands;
   end Load_Commands;

end Harriet.Commands.Loader;
