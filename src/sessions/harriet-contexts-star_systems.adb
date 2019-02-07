with Harriet.Star_Systems;
with Harriet.Contexts.Worlds;
with Harriet.Db.World;

package body Harriet.Contexts.Star_Systems is

   type Star_System_Context_Type is
     new Root_Context_Type with
      record
         Star_System : Harriet.Db.Star_System_Reference;
      end record;

   overriding function Class
     (Context : Star_System_Context_Type)
      return String
   is ("star-system");

   overriding function Name
     (Context : Star_System_Context_Type)
      return String;

   overriding function Is_Valid
     (Context : Star_System_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : Star_System_Context_Type;
      Children : in out Context_List'Class);

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Star_System_Context_Type;
      Children : in out Context_List'Class)
   is
   begin
      Children.Clear;
      for World of
        Harriet.Db.World.Select_By_Star_System
          (Context.Star_System)
      loop
         Children.Append
           (Harriet.Contexts.Worlds.World_Context (World.Reference));
      end loop;
   end Get_Child_Contexts;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Context : Star_System_Context_Type)
      return Boolean
   is
      use type Harriet.Db.Star_System_Reference;
   begin
      return Context.Star_System /= Harriet.Db.Null_Star_System_Reference;
   end Is_Valid;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Context : Star_System_Context_Type)
      return String
   is
   begin
      return Harriet.Star_Systems.Name (Context.Star_System);
   end Name;

   -------------------------
   -- Star_System_Context --
   -------------------------

   function Star_System_Context
     (Star_System : Harriet.Db.Star_System_Reference)
      return Context_Type
   is
   begin
      return Star_System_Context_Type'
        (Star_System => Star_System);
   end Star_System_Context;

end Harriet.Contexts.Star_Systems;
