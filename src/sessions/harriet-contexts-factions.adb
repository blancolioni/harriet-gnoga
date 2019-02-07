with Harriet.Factions;

package body Harriet.Contexts.Factions is

   type Faction_Context_Type is
     new Root_Context_Type with
      record
         Faction : Harriet.Db.Faction_Reference;
      end record;

   overriding function Is_Valid
     (Context : Faction_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : Faction_Context_Type;
      Children : in out Context_List'Class);

   overriding function Name
     (Context : Faction_Context_Type)
      return String
   is (Harriet.Factions.Name (Context.Faction));

   ---------------------
   -- Faction_Context --
   ---------------------

   function Faction_Context
     (Faction : Harriet.Db.Faction_Reference)
      return Context_Type
   is
   begin
      return Faction_Context_Type'
        (Root_Context_Type with Faction => Faction);
   end Faction_Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Faction_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
   end Get_Child_Contexts;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Context : Faction_Context_Type)
      return Boolean
   is
      use type Harriet.Db.Faction_Reference;
   begin
      return Context.Faction /= Harriet.Db.Null_Faction_Reference;
   end Is_Valid;

end Harriet.Contexts.Factions;
