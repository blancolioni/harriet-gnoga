with Harriet.Factions;

with Harriet.Contexts.Containers;
with Harriet.Contexts.Worlds;

with Harriet.Db.Faction;
with Harriet.Db.World;

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

   overriding function Class
     (Context : Faction_Context_Type)
      return String
   is ("faction");

   overriding function Name
     (Context : Faction_Context_Type)
      return String
   is (Harriet.Factions.Name (Context.Faction));

   procedure Scan_Worlds
     (Faction : Harriet.Db.Faction_Reference;
      Process : not null access
        procedure (Context : Context_Type));

   package Owned_World_Containers is
     new Harriet.Contexts.Containers
       (Container_Name    => "worlds",
        Context_Reference => Harriet.Db.Faction_Reference,
        Iterate_Children  => Scan_Worlds);

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
   begin
      Children.Clear;
      Children.Append
        (Owned_World_Containers.Container_Context (Context.Faction));
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

   -----------------
   -- Scan_Worlds --
   -----------------

   procedure Scan_Worlds
     (Faction : Harriet.Db.Faction_Reference;
      Process : not null access
        procedure (Context : Context_Type))
   is
   begin
      for World of
        Harriet.Db.World.Select_By_Owner
          (Harriet.Db.Faction.Get (Faction).Reference)
      loop
         Process (Harriet.Contexts.Worlds.World_Context (World.Reference));
      end loop;
   end Scan_Worlds;

end Harriet.Contexts.Factions;
