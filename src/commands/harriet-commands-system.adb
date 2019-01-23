with Harriet.UI.Gnoga_UI;

package body Harriet.Commands.System is

   type Stop_Server_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Execute
     (Command   : Stop_Server_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Stop_Server_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
   begin
      Harriet.UI.Gnoga_UI.Stop_Server
        (Argument (Arguments, "message", "stop server command"));
      Writer.Put_Line
        (Session.User_Name
         & ": server stopped via stop-server command");
   end Execute;

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Stop_Server : Stop_Server_Command;
   begin
      Register ("stop-server", Stop_Server);
   end Load_System_Commands;

end Harriet.Commands.System;
