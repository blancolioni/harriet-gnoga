with Harriet.Star_Systems;

with Harriet.UI.Models.Galaxy;
with Harriet.UI.Models.Star_System;
with Harriet.UI.Models.World;

with Harriet.UI.Views.Console;
with Harriet.UI.Views.Galaxy;
with Harriet.UI.Views.Star_System;
with Harriet.UI.Views.World;

with Harriet.UI.Views.Tables;

with Harriet.Db;
with Harriet.Db.World;

package body Harriet.Commands.Views is

   type Load_Galaxy_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type;

   type Load_Star_System_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type;

   type Load_World_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_World_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type;

   type Console_View_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Console_View_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type
   is
      pragma Unreferenced (Command);
   begin
      if Contains (Arguments, "table") then
         return Harriet.UI.Views.Tables.Create_Table_View
           (Harriet.UI.Models.Galaxy.Create_Galaxy_Model);
      else
         return Harriet.UI.Views.Galaxy.Galaxy_View
           (Harriet.UI.Models.Galaxy.Create_Galaxy_Model);
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type
   is
      pragma Unreferenced (Command);
   begin
      if not Contains (Arguments, "name") then
         return null;
      end if;

      declare
         use Harriet.Db;
         Star_System : constant Star_System_Reference :=
                         Harriet.Star_Systems.Find_Exact
                           (Argument (Arguments, "name"));
      begin
         if Star_System = Null_Star_System_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Harriet.UI.Views.Tables.Create_Table_View
              (Harriet.UI.Models.Star_System.Create (Star_System));
         else
            return Harriet.UI.Views.Star_System.Star_System_View
              (Harriet.UI.Models.Star_System.Create (Star_System));
         end if;
      end;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_World_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type
   is
      pragma Unreferenced (Command);
   begin
      if not Contains (Arguments, "star-system-name")
        or else not Contains (Arguments, "world-number")
      then
         return null;
      end if;

      declare
         use Harriet.Db;
         Star_System  : constant Star_System_Reference :=
                          Harriet.Star_Systems.Find_Exact
                            (Argument (Arguments, "star-system-name"));
         Reference    : World_Reference := Null_World_Reference;
         World_Number : constant Positive :=
                          Positive'Value
                            (Argument (Arguments, "world-number"));
         Index        : Natural := 0;
      begin

         if Star_System = Null_Star_System_Reference then
            return null;
         end if;

         for World of
           Harriet.Db.World.Select_By_Star_System (Star_System)
         loop
            Index := Index + 1;
            if Index = World_Number then
               Reference := World.Get_World_Reference;
               exit;
            end if;
         end loop;

         if Reference = Null_World_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Harriet.UI.Views.Tables.Create_Table_View
              (Harriet.UI.Models.World.Create (Reference),
               Headings_Down => True);
         else
            return Harriet.UI.Views.World.World_View
              (Harriet.UI.Models.World.Create (Reference));
         end if;
      end;

   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Console_View_Command;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Arguments);
   begin
      return Harriet.UI.Views.Console.Console_View;
   end Create_View;

   ------------------------
   -- Load_View_Commands --
   ------------------------

   procedure Load_View_Commands is
      Load_Galaxy      : Load_Galaxy_Command;
      Load_Star_System : Load_Star_System_Command;
      Load_World       : Load_World_Command;
      Console          : Console_View_Command;
   begin
      Register ("load-galaxy-view", Load_Galaxy);
      Register ("load-star-system-view", Load_Star_System);
      Register ("load-world-view", Load_World);
      Register ("console", Console);
   end Load_View_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      use Harriet.UI.Views;
      View : constant View_Type :=
               Load_View_Command'Class (Command).Create_View (Arguments);
   begin
      if View = null then
         Writer.Put_Error
           ("unable to create view");
      end if;

      Session.Main_View.Add_Child (View);
   end Perform;

end Harriet.Commands.Views;
