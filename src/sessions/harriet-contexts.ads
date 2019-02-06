with Harriet.Db;

package Harriet.Contexts is

   type Context_Type is private;

   function Show (Context : Context_Type) return String;

   function Is_Root (Context : Context_Type) return Boolean;
   function Root return Context_Type;

   procedure Initialize_Context
     (Context : in out Context_Type;
      Faction : Harriet.Db.Faction_Reference);

   procedure To_Parent (Context : in out Context_Type);

   procedure To_Child
     (Context : in out Context_Type;
      Child_Name : String;
      Success    : out Boolean);

   procedure Change_Scope
     (Context    : in out Context_Type;
      Scope_Path : String;
      Success    : out Boolean);

   procedure Iterate_Contents
     (Context : Context_Type;
      Process : not null access
        procedure (Item : Harriet.Db.Has_Name_Reference));

private

   type Context_Type is
      record
         Faction     : Harriet.Db.Faction_Reference :=
                         Harriet.Db.Null_Faction_Reference;
         Star_System : Harriet.Db.Star_System_Reference :=
                         Harriet.Db.Null_Star_System_Reference;
         World       : Harriet.Db.World_Reference :=
                         Harriet.Db.Null_World_Reference;
         Moon        : Harriet.Db.World_Reference :=
                         Harriet.Db.Null_World_Reference;
         Ship        : Harriet.Db.Ship_Reference :=
                         Harriet.Db.Null_Ship_Reference;
      end record;

   function Root return Context_Type is (others => <>);

end Harriet.Contexts;
