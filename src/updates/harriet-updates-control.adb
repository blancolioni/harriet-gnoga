with Harriet.Updates.Tasks;            use Harriet.Updates.Tasks;

package body Harriet.Updates.Control is

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

end Harriet.Updates.Control;
