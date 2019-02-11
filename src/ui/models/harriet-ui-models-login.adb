with Harriet.Db.User;

package body Harriet.UI.Models.Login is

   ------------------------
   -- Create_Login_Model --
   ------------------------

   function Create_Login_Model
     (Session : not null access Harriet.Sessions.Root_Harriet_Session'Class)
      return Login_Model
   is
   begin
      return new Root_Login_Model'
        (Watchers => <>,
         Session  => Harriet.Sessions.Harriet_Session (Session));
   end Create_Login_Model;

   -----------
   -- Login --
   -----------

   function Login
     (Model    : Root_Login_Model'Class;
      Name     : String;
      Password : String)
      return Boolean
   is
      User : constant Harriet.Db.User.User_Type :=
                  Harriet.Db.User.Get_By_Login (Name);
   begin
      if User.Has_Element
        and then User.Password = Password
      then
         Model.Session.Login (User.Get_User_Reference);
         return True;
      else
         return False;
      end if;
   end Login;

end Harriet.UI.Models.Login;
