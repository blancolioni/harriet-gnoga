with Harriet.Sessions;

package Harriet.UI.Models.Commands is

   type Root_Command_Type is abstract tagged private;
   type Command_Type is access all Root_Command_Type'Class;

   procedure On_Activated
     (Command : Root_Command_Type;
      Session : Harriet.Sessions.Harriet_Session;
      Value   : String)
   is abstract;

private

   type Root_Command_Type is abstract tagged null record;

end Harriet.UI.Models.Commands;
