with Ada.Text_IO;

with WL.Guids.Maps;

with Gnoga.Gui.Base;

with Harriet.UI.Models.Dashboard;
with Harriet.UI.Gnoga_UI.Views.Dashboard;

with Harriet.UI.Models.Login;
with Harriet.UI.Gnoga_UI.Views.Login;

with Harriet.Commands;

with Harriet.Db.User;
with Harriet.Db.Faction;

package body Harriet.Sessions is

   package Session_Maps is
     new WL.Guids.Maps (Harriet_Session);

   Map : Session_Maps.Map;

   procedure Show_Login_View
     (Session : not null access Root_Harriet_Session'Class);

   procedure On_Main_View_Destroyed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Main_Window_Key_Press
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record);

   -------------------
   -- Activate_View --
   -------------------

   procedure Activate_View
     (Session : in out Root_Harriet_Session'Class;
      View    : access Harriet.UI.Gnoga_UI.Views.Root_View_Type'Class)
   is
   begin
      Session.Active_View := View;
   end Activate_View;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
      return Harriet.Signals.Handler_Id
   is
   begin
      return Session.Dispatcher.Add_Handler (Signal, Handler, Data);
   end Add_Handler;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Signal : Harriet.Signals.Signal_Type) is
   begin
      for Session of Map loop
         Session.Send_Signal (Signal);
      end loop;
   end Broadcast;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Session     : not null access Root_Harriet_Session'Class;
      Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class)
   is
   begin
      Session.Main_Window := Main_Window;
      Main_Window.On_Key_Press_Handler (On_Main_Window_Key_Press'Access);
      Session.Show_Login_View;
   end Connect;

   -----------------
   -- End_Session --
   -----------------

   procedure End_Session (Session : in out Harriet_Session) is
   begin
      Ada.Text_IO.Put_Line
        ("ending session: "
         & WL.Guids.To_String (Session.Id));
      Map.Delete (Session.Id);
      Session := null;
   end End_Session;

   ------------------
   -- Find_Session --
   ------------------

   function Find_Session
     (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
      return Harriet_Session
   is
      use type Gnoga.Gui.View.Pointer_To_View_Base_Class;
   begin
      for Session of Map loop
         if Session.Main_View.Gnoga_View = Gnoga_View then
            return Session;
         end if;
      end loop;
      return null;
   end Find_Session;

   -----------
   -- Login --
   -----------

   procedure Login
     (Session : not null access Root_Harriet_Session'Class;
      User    : Harriet.Db.User_Reference)
   is
   begin
      Session.User := User;
      Session.Faction :=
        Harriet.Db.Faction.First_Reference_By_User (User);

      Ada.Text_IO.Put_Line
        ("session started for user " & Session.User_Name);
      declare
         Main_Model : constant Harriet.UI.Models.Dashboard.Dashboard_Model :=
                        Harriet.UI.Models.Dashboard.Create_Dashboard_Model
                          (Session);
         Main_View  : constant Harriet.UI.Gnoga_UI.Views.View_Type :=
                        Harriet.UI.Gnoga_UI.Views.Dashboard.Dashboard_View
                          (Main_Model);
      begin
         Session.Current_View.Gnoga_View.Visible (False);
         Main_View.Create (Session, Session.Main_Window.all, "dashboard");
         Session.Main_Window.Set_View (Main_View.Gnoga_View.all);
         Session.Current_View := Main_View;
         Main_View.Gnoga_View.On_Destroy_Handler
           (On_Main_View_Destroyed'Access);
         Main_View.Gnoga_View.Focus;
         Harriet.Commands.Execute_Command_Line
           ("load-galaxy-view",
            Harriet.Sessions.Harriet_Session (Session),
            Harriet.Commands.Null_Writer);
      end;

   end Login;

   ------------
   -- Logout --
   ------------

   procedure Logout (Session : not null access Root_Harriet_Session'Class) is
   begin
      Session.User := Harriet.Db.Null_User_Reference;
      Session.Show_Login_View;
   end Logout;

   -----------------
   -- New_Session --
   -----------------

   function New_Session return Harriet_Session is
   begin
      return Session : constant Harriet_Session :=
        new Root_Harriet_Session'
          (Id => WL.Guids.New_Guid,
           others => <>)
      do
         Map.Insert (Session.Id, Session);
         Ada.Text_IO.Put_Line
           ("new session: "
            & WL.Guids.To_String (Session.Id));
      end return;
   end New_Session;

   ----------------------------
   -- On_Main_View_Destroyed --
   ----------------------------

   procedure On_Main_View_Destroyed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      View : Gnoga.Gui.View.View_Type'Class renames
               Gnoga.Gui.View.View_Type'Class (Object);
      Session : Harriet_Session :=
                  Find_Session (View'Unchecked_Access);
   begin
      if Session /= null then
         End_Session (Session);
      end if;
   end On_Main_View_Destroyed;

   ------------------------------
   -- On_Main_Window_Key_Press --
   ------------------------------

   procedure On_Main_Window_Key_Press
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      Session : constant Harriet_Session :=
                  Harriet_Session (Object.Connection_Data);
   begin
      if not Session.Current_View.Accepts_Text_Entry then
         Session.Main_View.Handle_Key_Press
           (Meta    => Keyboard_Event.Meta,
            Alt     => Keyboard_Event.Alt,
            Control => Keyboard_Event.Control,
            Shift   => Keyboard_Event.Shift,
            Key     =>
              (if Keyboard_Event.Key_Code = 0
               then Wide_Character'Pos (Keyboard_Event.Key_Char)
               else Keyboard_Event.Key_Code));
      end if;
   end On_Main_Window_Key_Press;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id)
   is
   begin
      Session.Dispatcher.Remove_Handler (Signal, Id);
   end Remove_Handler;

   -----------------
   -- Send_Signal --
   -----------------

   overriding procedure Send_Signal
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type)
   is
   begin
      Session.Dispatcher.Call_Handlers (Session, Signal);
   end Send_Signal;

   ---------------------
   -- Show_Login_View --
   ---------------------

   procedure Show_Login_View
     (Session : not null access Root_Harriet_Session'Class)
   is
      Login_Model : constant Harriet.UI.Models.Login.Login_Model :=
                      Harriet.UI.Models.Login.Create_Login_Model
                        (Session);
      Login_View  : constant Harriet.UI.Gnoga_UI.Views.View_Type :=
                      Harriet.UI.Gnoga_UI.Views.Login.Login_View
                        (Login_Model);
   begin
      if Session.Current_View /= null then
         Session.Current_View.Gnoga_View.Visible (False);
      end if;
      Login_View.Create (Session, Session.Main_Window.all, "");
      Session.Main_Window.Set_View (Login_View.Gnoga_View.all);
      Session.Current_View := Login_View;
   end Show_Login_View;

   ---------------
   -- User_Name --
   ---------------

   function User_Name
     (Session : Root_Harriet_Session'Class)
      return String
   is
      use type Harriet.Db.User_Reference;
   begin
      if Session.User = Harriet.Db.Null_User_Reference then
         return "not logged in";
      else
         return Harriet.Db.User.Get (Session.User).Login;
      end if;
   end User_Name;

end Harriet.Sessions;