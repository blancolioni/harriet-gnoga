with Ada.Containers.Indefinite_Holders;
with Ada.Exceptions;
with Ada.Text_IO;

--  with Harriet.Employment;
with Harriet.Sessions;

package body Harriet.Updates.Tasks is

   package Signal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Harriet.Signals.Signal_Type, Harriet.Signals."=");

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
               begin
                  Update.Activate;
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "error while executing update: "
                        & Ada.Exceptions.Exception_Message (E));
               end;
            end loop;
--              Harriet.Employment.Execute_Employment_Contracts;
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Dispatch_Task;

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
               List  : Update_Lists.List;
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

end Harriet.Updates.Tasks;
