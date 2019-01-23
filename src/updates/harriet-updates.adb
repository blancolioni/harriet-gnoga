with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;

with Harriet.Sessions;
with Harriet.Signals;

with Harriet.Db.Calendar;

package body Harriet.Updates is

   package Update_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Harriet.Calendar.Time,
        Element_Type => Update_Interface'Class,
        "<"          => Harriet.Calendar."<");

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Update_Interface'Class);

   package Signal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Harriet.Signals.Signal_Type, Harriet.Signals."=");

   task Update_Task is
      entry Start;
      entry Stop;
      entry Add_Update
        (Clock  : Harriet.Calendar.Time;
         Update : Update_Interface'Class);
   end Update_Task;

   task Dispatch_Task is
      entry Dispatch (List : Update_Lists.List);
      entry Broadcast (Signal : Harriet.Signals.Signal_Type);
   end Dispatch_Task;

   -------------------
   -- Dispatch_Task --
   -------------------

   task body Dispatch_Task is
      Dispatch_List : Update_Lists.List;
      Signal_Holder : Signal_Holders.Holder;
   begin
      loop
         select
            accept Dispatch (List : in Update_Lists.List) do
               Dispatch_List := List;
            end Dispatch;
            for Update of Dispatch_List loop
               Update.Activate;
            end loop;
         or
            accept Broadcast (Signal : Harriet.Signals.Signal_Type) do
               Signal_Holder.Replace_Element (Signal);
            end Broadcast;
            Harriet.Sessions.Broadcast (Signal_Holder.Element);
         or
            terminate;
         end select;
      end loop;
   end Dispatch_Task;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
   begin
      Update_Task.Start;
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   procedure Stop_Updates is
   begin
      Update_Task.Stop;
   end Stop_Updates;

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Harriet.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Update_Task.Add_Update (Clock, Update);
   end Update_At;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
      Map : Update_Maps.Map;
   begin

      select
         accept Start;
      or
         terminate;
      end select;

      Ada.Text_IO.Put_Line ("Update task starting");

      declare
         Clock : constant Harriet.Db.Calendar.Calendar_Type :=
                   Harriet.Db.Calendar.First_By_Top_Record
                     (Harriet.Db.R_Calendar);
      begin
         Harriet.Calendar.Set_Clock (Clock.Clock);
      end;

      loop
         select
            accept Stop;
            exit;
         or
            accept Add_Update
              (Clock : in Harriet.Calendar.Time;
               Update : in Update_Interface'Class)
            do
               Map.Insert (Clock, Update);
            end Add_Update;
         else
            delay 0.1;
            Harriet.Calendar.Advance (3600.0);
            Dispatch_Task.Broadcast
              (Harriet.Sessions.Signal_Clock_Tick);

            declare
               Clock : constant Harriet.Db.Calendar.Calendar_Type :=
                         Harriet.Db.Calendar.First_By_Top_Record
                           (Harriet.Db.R_Calendar);
            begin
               Clock.Set_Clock (Harriet.Calendar.Clock);
            end;

            declare
               use type Harriet.Calendar.Time;
               List : Update_Lists.List;
               Clock : constant Harriet.Calendar.Time :=
                         Harriet.Calendar.Clock;
            begin
               while not Map.Is_Empty
                 and then Map.First_Key >= Clock
               loop
                  List.Append (Map.First_Element);
                  Map.Delete_First;
               end loop;
               Dispatch_Task.Dispatch (List);
            end;
         end select;
      end loop;
      Ada.Text_IO.Put_Line ("Update task stopping");
   end Update_Task;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class)
   is
      use type Harriet.Calendar.Time;
   begin
      Update_Task.Add_Update (Harriet.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Harriet.Updates;
