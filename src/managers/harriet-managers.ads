with Harriet.Calendar;

with Harriet.Db;

package Harriet.Managers is

   type Root_Manager_Type is abstract tagged private;

   procedure Activate
     (Manager : not null access Root_Manager_Type)
   is abstract;

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Harriet.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Constructor_Function is access
     function (Managed : Harriet.Db.Managed_Reference)
               return Manager_Type;

   procedure Register_Manager
     (Name        : String;
      Constructor : Constructor_Function);

   function Get_Manager
     (Managed : Harriet.Db.Managed_Reference;
      Name    : String)
      return Manager_Type;

   procedure Load_Managers;

private

   type Root_Manager_Type is abstract tagged
      record
         Managed         : Harriet.Db.Managed_Reference;
         Is_Active       : Boolean := False;
         Has_Next_Update : Boolean := False;
         Next_Update     : Harriet.Calendar.Time;
      end record;

end Harriet.Managers;
