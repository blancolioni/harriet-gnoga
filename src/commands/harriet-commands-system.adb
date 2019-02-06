with Ada.Strings.Fixed;

with Harriet.Contexts;

with Harriet.UI.Gnoga_UI;

package body Harriet.Commands.System is

   type Stop_Server_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Execute
     (Command   : Stop_Server_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   type Change_Scope_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Execute
     (Command   : Change_Scope_Command;
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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Change_Scope_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
      Context : Harriet.Contexts.Context_Type := Session.Current_Context;
   begin
      if Argument_Count (Arguments) /= 1 then
         Writer.Put_Error ("Usage: cd <path>");
         return;
      end if;

      declare
         use Ada.Strings.Fixed;
         Path : constant String := Argument (Arguments, 1) & "/";
         Start : Positive := Path'First;
         Finish : Natural := Index (Path, "/", Start);
      begin
         while Finish > 0 loop
            declare
               Element : constant String := Path (Start .. Finish - 1);
            begin
               if Element = "" then
                  null;
               elsif Element = "." then
                  null;
               elsif Element = ".." then
                  if Harriet.Contexts.Is_Root (Context) then
                     null;
                  else
                     Harriet.Contexts.To_Parent (Context);
                  end if;
               else
                  declare
                     Success : Boolean;
                  begin
                     Harriet.Contexts.To_Child (Context, Element, Success);
                     if not Success then
                        Writer.Put_Error
                          ("No such context: "
                           & Path (Path'First .. Finish - 1));
                        return;
                     end if;
                  end;
               end if;
            end;
            Start := Finish + 1;
            Finish := Index (Path, "/", Start);
         end loop;

         Session.Update_Context (Context);
      end;
   end Execute;

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Change_Scope : Change_Scope_Command;
      Stop_Server  : Stop_Server_Command;
   begin
      Register ("cd", Change_Scope);
      Register ("stop-server", Stop_Server);
   end Load_System_Commands;

end Harriet.Commands.System;
