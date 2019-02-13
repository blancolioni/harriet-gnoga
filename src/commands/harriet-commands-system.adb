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

package body Harriet.Commands.System is

   type Cat_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
     (Command   : Cat_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   type Echo_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
     (Command   : Echo_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   type Change_Scope_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
     (Command   : Change_Scope_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   type List_Command is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
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

   overriding function Administrator_Only
     (Command : Status_Command)
      return Boolean
   is (True);

   overriding procedure Perform
     (Command   : Status_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Cat                   : Cat_Command;
      Change_Scope          : Change_Scope_Command;
      Echo                  : Echo_Command;
      List                  : List_Command;
      Pause_Command         : Status_Command (Pause_Server);
      Resume_Command        : Status_Command (Resume_Server);
      Stop_Command          : Status_Command (Stop_Server);
      Get_Status_Command    : Status_Command (Show_Status);
      Get_Db_Status_Command : Status_Command (Show_Database_Statistics);
      Update_Speed_Command  : Status_Command (Update_Speed);
   begin
      Register ("cat", Cat);
      Register ("cd", Change_Scope);
      Register ("change-scope", Change_Scope);
      Register ("echo", Echo);
      Register ("ls", List);
      Register ("pause", Pause_Command);
      Register ("resume", Resume_Command);
      Register ("update-speed", Update_Speed_Command);
      Register ("stop-server", Stop_Command);
      Register ("status", Get_Status_Command);
      Register ("db-status", Get_Db_Status_Command);
   end Load_System_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Cat_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
   begin
      if Argument_Count (Arguments) = 0 then
         Writer.Put_Error ("Usage: cat file [ files ... ]");
         return;
      end if;

      for I in 1 .. Argument_Count (Arguments) loop
         declare
            Context : constant Harriet.Contexts.Context_Type :=
                        Session.Current_Context.Go
                          (Argument (Arguments, I));
         begin
            if not Context.Is_Valid then
               Writer.Put_Error (Argument (Arguments, I) & ": not found");
            else
               Writer.Put_Line
                 (Context.Get_Content);
            end if;
         end;
      end loop;
   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Change_Scope_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
      Context : constant Harriet.Contexts.Context_Path :=
                  Session.Current_Context;
   begin
      if Argument_Count (Arguments) = 0 then
         Session.Update_Context
           (Harriet.Contexts.Initial_Context_Path (Session.Faction));
         return;
      end if;

      if Argument_Count (Arguments) /= 1 then
         Writer.Put_Error ("Usage: cd <path>");
         return;
      end if;

      declare
         Scope   : constant String := Argument (Arguments, 1);
         New_Context : constant Harriet.Contexts.Context_Path :=
                         Context.Go (Scope);
      begin
         if not New_Context.Context.Is_Valid then
            Writer.Put_Error
              ("Invalid context: " & Scope & ": "
               & New_Context.Context.Name);
         else
            Session.Update_Context (New_Context);
         end if;

      end;

   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Echo_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Session);
   begin
      for I in 1 .. Argument_Count (Arguments) loop
         if I > 1 then
            Writer.Put (" ");
         end if;
         Writer.Put (Argument (Arguments, I));
      end loop;

      if not Contains (Arguments, "n") then
         Writer.New_Line;
      end if;

   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : List_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);

      Ids : Identifier_List;

      procedure Put_Item (Item : String);

      --------------
      -- Put_Item --
      --------------

      procedure Put_Item (Item : String) is
      begin
         Add (Ids, Item);
      end Put_Item;

   begin
      if Argument_Count (Arguments) = 0 then
         Session.Current_Context.Context.Iterate_Child_Names
           (Put_Item'Access);
         Writer.Put_Identifier_List (Ids);
      elsif Argument_Count (Arguments) = 1 then
         declare
            Context : constant Harriet.Contexts.Context_Path :=
                        Session.Current_Context.Go (Argument (Arguments, 1));
         begin
            if not Context.Context.Is_Valid then
               Writer.Put_Error
                 ("Cannot list " & Argument (Arguments, 1));
            else
               Harriet.Contexts.Iterate_Child_Names
                 (Context, Put_Item'Access);
               Writer.Put_Identifier_List (Ids);
            end if;
         end;
      else
         Writer.Put_Error
           ("Usage: ls [scope-path]");
      end if;
   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
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
                 ("new blocks:   " & Info.Blocks'Image);
               Writer.Put_Line
                 ("cached pages: " & Info.Pages'Image);
               Writer.Put_Line
                 ("total records:" & Info.Record_Count'Image);
               Writer.Put_Line
                 ("cache hits:   " & Info.Hits'Image);
               Writer.Put_Line
                 ("cache misses: " & Info.Misses'Image);
               Writer.Put_Line
                 ("file reads:   " & Info.Reads'Image);
               Writer.Put_Line
                 ("file writes:  " & Info.Writes'Image);
            end;
      end case;
   end Perform;

end Harriet.Commands.System;
