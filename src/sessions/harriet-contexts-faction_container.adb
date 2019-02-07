with Harriet.Db.Faction;

with Harriet.Contexts.Factions;

package body Harriet.Contexts.Faction_Container is

   type Faction_Container_Context_Type is
     new Root_Context_Type with null record;

   overriding function Name
     (Context : Faction_Container_Context_Type)
      return String
   is ("factions");

   overriding function Is_Valid
     (Context : Faction_Container_Context_Type)
      return Boolean
   is (True);

   overriding procedure Get_Child_Contexts
     (Context  : Faction_Container_Context_Type;
      Children : in out Context_List'Class);

   -------------------------------
   -- Faction_Container_Context --
   -------------------------------

   function Faction_Container_Context
      return Context_Type
   is
   begin
      return Context : Faction_Container_Context_Type;
   end Faction_Container_Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Faction_Container_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
      for Faction of Harriet.Db.Faction.Scan_By_Name loop
         Children.Append
           (Harriet.Contexts.Factions.Faction_Context
              (Faction.Reference));
      end loop;
   end Get_Child_Contexts;

end Harriet.Contexts.Faction_Container;
