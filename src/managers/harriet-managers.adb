with Harriet.Logging;
with Harriet.Updates.Events;

package body Harriet.Managers is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
   begin
      Update.Manager.Has_Next_Update := False;
      Update.Manager.Activate;
      declare
         Rec : constant Harriet.Db.Managed.Managed_Type :=
                 Harriet.Db.Managed.Get
                   (Update.Manager.Managed);
      begin
         if Update.Manager.Has_Next_Update then
            Update.Manager.Is_Active := True;
            Harriet.Updates.Events.Update_At
              (Clock  => Update.Manager.Next_Update,
               Update => Update);
            Rec.Set_Next_Event (Update.Manager.Next_Update);
            Rec.Set_Active (True);
         else
            Harriet.Logging.Log
              (Actor    => "update",
               Location => "",
               Category => "",
               Message  => "deactivating");
            Rec.Set_Active (False);
         end if;
      end;
   end Activate;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration)
   is
      use type Harriet.Calendar.Time;
   begin
      Manager.Set_Next_Update_Time (Harriet.Calendar.Clock + Update_Delay);
   end Set_Next_Update_Delay;

   --------------------------
   -- Set_Next_Update_Time --
   --------------------------

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Harriet.Calendar.Time)
   is
   begin
      if Manager.Is_Active then
         Manager.Has_Next_Update := True;
         Manager.Next_Update := Update_Time;
      else
         declare
            Update : constant Manager_Update :=
                       (Manager => Manager_Type (Manager));
         begin
            Manager.Is_Active := True;
            Harriet.Updates.Events.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;

   end Set_Next_Update_Time;

end Harriet.Managers;
