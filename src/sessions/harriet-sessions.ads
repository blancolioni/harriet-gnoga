with Gnoga.Types;

private with Gnoga.Gui.View;
with Gnoga.Gui.Window;

private with WL.Guids;

limited with Harriet.UI.Views;

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

   type Harriet_Session is access all Root_Harriet_Session'Class;

   function New_Session return Harriet_Session;
   procedure End_Session (Session : in out Harriet_Session);

   procedure Broadcast (Signal : Harriet.Signals.Signal_Type);

private

   type Root_Harriet_Session is
     new Gnoga.Types.Connection_Data_Type
     and Harriet.Signals.Signaler with
      record
         Id            : WL.Guids.Guid;
         Main_Window   : Gnoga.Gui.Window.Pointer_To_Window_Class;
         User          : Harriet.Db.User_Reference :=
                           Harriet.Db.Null_User_Reference;
         Faction       : Harriet.Db.Faction_Reference :=
                           Harriet.Db.Null_Faction_Reference;
         Current_View  : access Harriet.UI.Views.Root_View_Type'Class;
         Active_View   : access Harriet.UI.Views.Root_View_Type'Class;
         Dispatcher    : Harriet.Signals.Signal_Dispatcher;
      end record;

   function Faction
     (Session : Root_Harriet_Session'Class)
      return Harriet.Db.Faction_Reference
   is (Session.Faction);

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
