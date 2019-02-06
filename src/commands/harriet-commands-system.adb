with Harriet.Contexts;

with Harriet.UI.Gnoga_UI;

with Harriet.Db.Has_Name;

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

   type List_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Execute
     (Command   : List_Command;
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
      if Argument_Count (Arguments) = 0 then
         Harriet.Contexts.Initialize_Context (Context, Session.Faction);
         Session.Update_Context (Context);
         return;
      end if;

      if Argument_Count (Arguments) /= 1 then
         Writer.Put_Error ("Usage: cd <path>");
         return;
      end if;

      declare
         Success : Boolean;
         Scope   : constant String := Argument (Arguments, 1);
      begin
         Harriet.Contexts.Change_Scope (Context, Scope, Success);
         if not Success then
            Writer.Put_Error
              ("No such context: " & Scope);
            return;
         end if;
      end;

      Session.Update_Context (Context);

   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : List_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);

      procedure Put_Item (Item : Harriet.Db.Has_Name_Reference);

      --------------
      -- Put_Item --
      --------------

      procedure Put_Item (Item : Harriet.Db.Has_Name_Reference) is
      begin
         Writer.Put_Line
           (Harriet.Db.Has_Name.Get (Item).Name);
      end Put_Item;

   begin
      if Argument_Count (Arguments) = 0 then
         Harriet.Contexts.Iterate_Contents
           (Session.Current_Context, Put_Item'Access);
      elsif Argument_Count (Arguments) = 1 then
         declare
            Success : Boolean;
            Context : Harriet.Contexts.Context_Type :=
                        Session.Current_Context;
         begin
            Harriet.Contexts.Change_Scope
              (Context, Argument (Arguments, 1), Success);
            if not Success then
               Writer.Put_Error
                 ("Cannot list " & Argument (Arguments, 1));
            else
               Harriet.Contexts.Iterate_Contents
                 (Session.Current_Context, Put_Item'Access);
            end if;
         end;
      else
         Writer.Put_Error
           ("Usage: ls [scope-path]");
      end if;
   end Execute;

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Change_Scope : Change_Scope_Command;
      List         : List_Command;
      Stop_Server  : Stop_Server_Command;
   begin
      Register ("cd", Change_Scope);
      Register ("change-scope", Change_Scope);
      Register ("ls", List);
      Register ("stop-server", Stop_Server);
   end Load_System_Commands;

end Harriet.Commands.System;
