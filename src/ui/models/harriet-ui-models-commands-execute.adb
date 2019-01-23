with Harriet.Commands;

package body Harriet.UI.Models.Commands.Execute is

   type Execute_Command_Line_Record is
     new Root_Command_Type with null record;

   overriding procedure On_Activated
     (Command : Execute_Command_Line_Record;
      Session : Harriet.Sessions.Harriet_Session;
      Value   : String);

   Singleton : aliased Execute_Command_Line_Record;

   --------------------------
   -- Execute_Command_Line --
   --------------------------

   function Execute_Command_Line return Command_Type is
   begin
      return Singleton'Access;
   end Execute_Command_Line;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Command : Execute_Command_Line_Record;
      Session : Harriet.Sessions.Harriet_Session;
      Value   : String)
   is
      pragma Unreferenced (Command);
   begin
      Harriet.Commands.Execute_Command_Line
        (Line    => Value,
         Session => Session,
         Writer  => Harriet.Commands.Null_Writer);
   end On_Activated;

end Harriet.UI.Models.Commands.Execute;
