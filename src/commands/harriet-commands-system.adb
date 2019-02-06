with Ada.Calendar.Formatting;

with Marlowe.Version;
with Kit.Version;

with Harriet.Version;

with Harriet.Calendar;
with Harriet.Contexts;
with Harriet.Updates.Control;

with Harriet.UI.Gnoga_UI;

with Marlowe.Database;
with Harriet.Db.Marlowe_Keys;

with Harriet.Db.Has_Name;

package body Harriet.Commands.System is

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

   type Status_Command_Type is
     (Pause_Server, Resume_Server, Stop_Server,
      Update_Speed,
      Show_Status, Show_Database_Statistics);

   type Status_Command (Command : Status_Command_Type) is
     new Root_Harriet_Command with null record;

   overriding procedure Execute
     (Command   : Status_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Status_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
   begin
      case Command.Command is
         when Pause_Server =>
            Harriet.Updates.Control.Pause_Updates;
         when Resume_Server =>
            Harriet.Updates.Control.Resume_Updates;
         when Stop_Server =>
            Harriet.UI.Gnoga_UI.Stop_Server
              (Argument (Arguments, "message", "stop server command"));
            Writer.Put_Line
              (Session.User_Name
               & ": server stopped via stop-server command");
         when Update_Speed =>

            if Argument_Count (Arguments) > 1 then
               Writer.Put_Error ("Usage: update-speed [time factor]");
               return;
            end if;

            if Argument_Count (Arguments) = 1 then
               declare
                  Value : Duration;
               begin
                  Value := Duration'Value (Argument (Arguments, 1));
                  Harriet.Updates.Control.Set_Advance_Speed (Value);
               exception
                  when Constraint_Error =>
                     Writer.Put_Error ("Usage: update-speed [time factor]");
                     return;
               end;
            end if;

            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Harriet.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

         when Show_Status =>
            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Writer.Put_Line ("logged in as " & Session.User_Name);
               Harriet.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 (Harriet.Version.Name
                  & " version "
                  & Harriet.Version.Version_String);
               Writer.Put_Line
                 ("kit     "
                  & Kit.Version.Version_String);
               Writer.Put_Line
                 ("marlowe "
                  & Marlowe.Version.Version_String);
               Writer.Put_Line
                  ("Server started "
                   & Ada.Calendar.Formatting.Image
                     (Start_Time));
               Writer.Put_Line
                 ("status: " & (if Paused then "paused" else "running"));
               Writer.Put_Line
                 ("current server date: "
                  & Harriet.Calendar.Image
                    (Harriet.Calendar.Clock));
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

         when Show_Database_Statistics =>
            declare
               use Harriet.Db.Marlowe_Keys;
               Info : constant Marlowe.Database.Database_Information :=
                        Handle.Get_Data_Store_Information;
            begin
               Writer.Put_Line
                 ("blocks:   " & Info.Blocks'Image);
               Writer.Put_Line
                 ("pages:    " & Info.Pages'Image);
               Writer.Put_Line
                 ("records:  " & Info.Record_Count'Image);
               Writer.Put_Line
                 ("hits:     " & Info.Hits'Image);
               Writer.Put_Line
                 ("misses:   " & Info.Misses'Image);
               Writer.Put_Line
                 ("reads:    " & Info.Reads'Image);
               Writer.Put_Line
                 ("writes:   " & Info.Writes'Image);
            end;
      end case;
   end Execute;

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Change_Scope          : Change_Scope_Command;
      List                  : List_Command;
      Pause_Command         : Status_Command (Pause_Server);
      Resume_Command        : Status_Command (Resume_Server);
      Stop_Command          : Status_Command (Stop_Server);
      Get_Status_Command    : Status_Command (Show_Status);
      Get_Db_Status_Command : Status_Command (Show_Database_Statistics);
      Update_Speed_Command  : Status_Command (Update_Speed);
   begin
      Register ("cd", Change_Scope);
      Register ("change-scope", Change_Scope);
      Register ("ls", List);
      Register ("pause", Pause_Command);
      Register ("resume", Resume_Command);
      Register ("update-speed", Update_Speed_Command);
      Register ("stop-server", Stop_Command);
      Register ("status", Get_Status_Command);
      Register ("db-status", Get_Db_Status_Command);
   end Load_System_Commands;

end Harriet.Commands.System;
