with Harriet.Calendar;

package Harriet.Updates is

   type Update_Interface is interface;

   procedure Activate
     (Update : Update_Interface)
   is abstract;

   procedure Update_At
     (Clock  : Harriet.Calendar.Time;
      Update : Update_Interface'Class);

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class);

   procedure Start_Updates;
   procedure Stop_Updates;

end Harriet.Updates;
