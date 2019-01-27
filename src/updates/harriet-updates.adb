with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;

with Harriet.Sessions;
with Harriet.Signals;

package body Harriet.Updates is

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Update_Interface'Class);

   package Update_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Harriet.Calendar.Time,
        Element_Type => Update_Lists.List,
        "<"          => Harriet.Calendar."<",
        "="          => Update_Lists."=");

   package Signal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Harriet.Signals.Signal_Type, Harriet.Signals."=");

   task Update_Task is
      entry Start;
      entry Stop;
   end Update_Task;

   task Broadcast_Task is
      entry Broadcast (Signal : Harriet.Signals.Signal_Type);
      entry Stop;
   end Broadcast_Task;

   task Dispatch_Task is
      entry Dispatch (List : Update_Lists.List);
      entry Stop;
   end Dispatch_Task;

   protected Update_Map is

      procedure Add_Update
        (Clock  : Harriet.Calendar.Time;
         Update : Update_Interface'Class);

      procedure Get_Updates
        (Clock : Harriet.Calendar.Time;
         List  : out Update_Lists.List);

   private

      Map : Update_Maps.Map;

   end Update_Map;

   --------------------
   -- Broadcast_Task --
   --------------------

   task body Broadcast_Task is
      Signal_Holder : Signal_Holders.Holder;
   begin
      loop
         select
            accept Broadcast (Signal : Harriet.Signals.Signal_Type) do
               Signal_Holder.Replace_Element (Signal);
            end Broadcast;
            Harriet.Sessions.Broadcast (Signal_Holder.Element);
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Broadcast_Task;

   -------------------
   -- Dispatch_Task --
   -------------------

   task body Dispatch_Task is
      Dispatch_List : Update_Lists.List;
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
            accept Stop;
            exit;
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
      Dispatch_Task.Stop;
      Broadcast_Task.Stop;
   end Stop_Updates;

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Harriet.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Update_Map.Add_Update (Clock, Update);
   end Update_At;

   ----------------
   -- Update_Map --
   ----------------

   protected body Update_Map is

      ----------------
      -- Add_Update --
      ----------------

      procedure Add_Update
        (Clock  : Harriet.Calendar.Time;
         Update : Update_Interface'Class)
      is
      begin
         if Map.Contains (Clock) then
            Map (Clock).Append (Update);
         else
            declare
               List : Update_Lists.List;
            begin
               List.Append (Update);
               Map.Insert (Clock, List);
            end;
         end if;
      end Add_Update;

      -----------------
      -- Get_Updates --
      -----------------

      procedure Get_Updates
        (Clock : Harriet.Calendar.Time;
         List  : out Update_Lists.List)
      is
         use type Harriet.Calendar.Time;
      begin
         while not Map.Is_Empty
           and then Map.First_Key <= Clock
         loop
            for Upd of Map.First_Element loop
               List.Append (Upd);
            end loop;
            Map.Delete_First;
         end loop;
      end Get_Updates;

   end Update_Map;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
   begin

      select
         accept Start;
      or
         terminate;
      end select;

      Ada.Text_IO.Put_Line ("Update task starting");

      loop
         select
            accept Stop;
            exit;
         else
            delay 0.1;
            Harriet.Calendar.Advance (3600.0);
            Broadcast_Task.Broadcast
              (Harriet.Sessions.Signal_Clock_Tick);

            declare
               List : Update_Lists.List;
               Clock : constant Harriet.Calendar.Time :=
                         Harriet.Calendar.Clock;
            begin
               Update_Map.Get_Updates (Clock, List);

               if not List.Is_Empty then
                  Dispatch_Task.Dispatch (List);
               end if;
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
      Update_Map.Add_Update (Harriet.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Harriet.Updates;
