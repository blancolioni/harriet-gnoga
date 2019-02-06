private with Ada.Containers.Doubly_Linked_Lists;

with Gnoga.Types;

private with Gnoga.Gui.View;
with Gnoga.Gui.Window;

private with WL.Guids;

limited with Harriet.UI.Views;

with Harriet.Contexts;
with Harriet.Signals;

with Harriet.Db;

package Harriet.Sessions is

   Signal_Clock_Tick : constant Harriet.Signals.Signal_Type :=
                         "signal-clock-tick";

   type Root_Harriet_Session is
     new Gnoga.Types.Connection_Data_Type
     and Harriet.Signals.Signaler
   with private;

   overriding procedure Send_Signal
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type);

   overriding function Add_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
     return Harriet.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id);

   procedure Connect
     (Session     : not null access Root_Harriet_Session'Class;
      Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class);

   procedure Login
     (Session : not null access Root_Harriet_Session'Class;
      User    : Harriet.Db.User_Reference);

   procedure Logout (Session : not null access Root_Harriet_Session'Class);

   function User_Name
     (Session : Root_Harriet_Session'Class)
      return String;

   function Faction
     (Session : Root_Harriet_Session'Class)
      return Harriet.Db.Faction_Reference;

   function Main_View
     (Session : Root_Harriet_Session'Class)
      return access Harriet.UI.Views.Root_View_Type'Class;

   function Active_View
     (Session : Root_Harriet_Session'Class)
      return access Harriet.UI.Views.Root_View_Type'Class;

   procedure Activate_View
     (Session : in out Root_Harriet_Session'Class;
      View    : access Harriet.UI.Views.Root_View_Type'Class);

   function Current_Context
     (Session : Root_Harriet_Session'Class)
      return Harriet.Contexts.Context_Type;

   procedure Update_Context
     (Session : in out Root_Harriet_Session'Class;
      Context : Harriet.Contexts.Context_Type);

   type Harriet_Session is access all Root_Harriet_Session'Class;

   function New_Gnoga_Session return Harriet_Session;
   function New_Repl_Session
     (User : Harriet.Db.User_Reference)
      return Harriet_Session;

   procedure End_Session (Session : in out Harriet_Session);

   procedure End_All_Sessions;

   procedure Broadcast (Signal : Harriet.Signals.Signal_Type);

private

   type View_Access is access all Harriet.UI.Views.Root_View_Type'Class;

   package View_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (View_Access);

   type Root_Harriet_Session is
     new Gnoga.Types.Connection_Data_Type
     and Harriet.Signals.Signaler with
      record
         Id            : WL.Guids.Guid;
         Is_Gnoga      : Boolean := False;
         Main_Window   : Gnoga.Gui.Window.Pointer_To_Window_Class;
         User          : Harriet.Db.User_Reference :=
                           Harriet.Db.Null_User_Reference;
         Faction       : Harriet.Db.Faction_Reference :=
                           Harriet.Db.Null_Faction_Reference;
         Context       : Harriet.Contexts.Context_Type;
         Current_View  : View_Access;
         Active_View   : View_Access;
         Views         : View_Lists.List;
         Dispatcher    : Harriet.Signals.Signal_Dispatcher;
      end record;

   function Faction
     (Session : Root_Harriet_Session'Class)
      return Harriet.Db.Faction_Reference
   is (Session.Faction);

   function Current_Context
     (Session : Root_Harriet_Session'Class)
      return Harriet.Contexts.Context_Type
   is (Session.Context);

   function Main_View
     (Session : Root_Harriet_Session'Class)
      return access Harriet.UI.Views.Root_View_Type'Class
   is (Session.Current_View);

   function Active_View
     (Session : Root_Harriet_Session'Class)
      return access Harriet.UI.Views.Root_View_Type'Class
   is (Session.Active_View);

   function Find_Session
     (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
      return Harriet_Session;

end Harriet.Sessions;
