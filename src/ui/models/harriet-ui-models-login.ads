with Harriet.Sessions;

package Harriet.UI.Models.Login is

   type Root_Login_Model is
     new Root_Harriet_Model with private;

   function Login
     (Model    : Root_Login_Model'Class;
      Name     : String;
      Password : String)
      return Boolean;

   type Login_Model is access all Root_Login_Model'Class;

   function Create_Login_Model
     (Session : not null access Harriet.Sessions.Root_Harriet_Session'Class)
      return Login_Model;

private

   type Root_Login_Model is
     new Root_Harriet_Model with
      record
         Session : Harriet.Sessions.Harriet_Session;
      end record;

   overriding function Title
     (Model : Root_Login_Model)
      return String
   is ("Login");

end Harriet.UI.Models.Login;
