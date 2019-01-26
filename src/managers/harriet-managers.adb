with Ada.Text_IO;

with WL.String_Maps;

with Harriet.Updates;

with Harriet.Db.Managed;

package body Harriet.Managers is

   type Manager_Update is
     new Harriet.Updates.Update_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Activate
     (Update : Manager_Update);

   package Register_Maps is
     new WL.String_Maps (Constructor_Function);

   Register : Register_Maps.Map;

   package Manager_Maps is
     new WL.String_Maps (Manager_Type);

   Active_Map : Manager_Maps.Map;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
   begin
      Update.Manager.Has_Next_Update := False;
      Update.Manager.Activate;
      if Update.Manager.Has_Next_Update then
         Update.Manager.Is_Active := True;
         Harriet.Updates.Update_At
           (Clock  => Update.Manager.Next_Update,
            Update => Update);
      else
         declare
            Rec : constant Harriet.Db.Managed.Managed_Type :=
                    Harriet.Db.Managed.Get
                      (Update.Manager.Managed);
         begin
            Rec.Set_Active (False);
         end;
      end if;
   end Activate;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (Managed : Harriet.Db.Managed_Reference;
      Name    : String)
      return Manager_Type
   is
      Key : constant String := Name & Harriet.Db.To_String (Managed);
   begin
      if not Active_Map.Contains (Key) then
         return null;
      else
         return Active_Map.Element (Key);
      end if;
   end Get_Manager;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
   begin
      for Managed of Harriet.Db.Managed.Scan_By_Top_Record loop
         if Register.Contains (Managed.Manager) then
            declare
               Ref     : constant Harriet.Db.Managed_Reference :=
                           Managed.Reference;
               Key : constant String :=
                       Managed.Manager & Harriet.Db.To_String (Ref);
               Manager : constant Manager_Type :=
                           Register.Element (Managed.Manager) (Ref);

            begin
               if Manager /= null then
                  Manager.Managed := Ref;
                  Active_Map.Insert (Key, Manager);
                  if Managed.Active then
                     declare
                        Update : constant Manager_Update :=
                                   (Manager => Manager);
                     begin
                        Ada.Text_IO.Put_Line
                          (Key & " activating at "
                           & Harriet.Calendar.Image (Managed.Next_Event));
                        Harriet.Updates.Update_At
                          (Clock  => Managed.Next_Event,
                           Update => Update);
                     end;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Load_Managers;

   ----------------------
   -- Register_Manager --
   ----------------------

   procedure Register_Manager
     (Name        : String;
      Constructor : Constructor_Function)
   is
   begin
      Register.Insert (Name, Constructor);
   end Register_Manager;

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
            Harriet.Updates.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;
   end Set_Next_Update_Time;

end Harriet.Managers;
