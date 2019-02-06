with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;

with Harriet.Factions;
with Harriet.Star_Systems;
with Harriet.Worlds;

with Harriet.Db.Star_System;
with Harriet.Db.Star_System_Object;
with Harriet.Db.Faction;
with Harriet.Db.World;

package body Harriet.Contexts is

   ------------------
   -- Change_Scope --
   ------------------

   procedure Change_Scope
     (Context    : in out Context_Type;
      Scope_Path : String;
      Success    : out Boolean)
   is
      use Ada.Strings.Fixed;
      Path   : constant String := Scope_Path & "/";
      Start  : Positive := Path'First;
      Finish : Natural := Index (Path, "/", Start);
   begin

      Success := True;

      if Path (Path'First) = '/' then
         Context := Harriet.Contexts.Root;
      end if;

      while Finish > 0 loop
         declare
            Element : constant String := Path (Start .. Finish - 1);
         begin
            if Element = "" then
               null;
            elsif Element = "." then
               null;
            elsif Element = ".." then
               if Harriet.Contexts.Is_Root (Context) then
                  null;
               else
                  Harriet.Contexts.To_Parent (Context);
               end if;
            else
               Harriet.Contexts.To_Child (Context, Element, Success);
               if not Success then
                  return;
               end if;
            end if;
         end;
         Start := Finish + 1;
         Finish := Index (Path, "/", Start);
      end loop;

   end Change_Scope;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Context : in out Context_Type;
      Faction : Harriet.Db.Faction_Reference)
   is
   begin
      Context.Faction := Faction;
      Context.Star_System :=
        Harriet.Factions.Capital_System (Context.Faction);
      Context.World :=
        Harriet.Factions.Capital_World (Context.Faction);
   end Initialize_Context;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Context : Context_Type) return Boolean is
   begin
      return Show (Context) = "/";
   end Is_Root;

   ----------------------
   -- Iterate_Contents --
   ----------------------

   procedure Iterate_Contents
     (Context : Context_Type;
      Process : not null access
        procedure (Item : Harriet.Db.Has_Name_Reference))
   is
      use Harriet.Db;

      package Ref_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Harriet.Db.Has_Name_Reference, Harriet.Db."=");

      Refs : Ref_Lists.List;

      procedure Get_References
        (Primary : Harriet.Db.Star_System_Object_Reference);

      --------------------
      -- Get_References --
      --------------------

      procedure Get_References
        (Primary : Harriet.Db.Star_System_Object_Reference)
      is
      begin
         for Object of
           Harriet.Db.Star_System_Object.Select_By_Primary (Primary)
         loop
            Refs.Append (Object.Reference);
         end loop;
      end Get_References;

   begin
      if Context.Moon /= Null_World_Reference then
         Get_References (Harriet.Db.World.Get (Context.Moon).Reference);
      elsif Context.World /= Null_World_Reference then
         Get_References (Harriet.Db.World.Get (Context.World).Reference);
      elsif Context.Star_System /= Null_Star_System_Reference then
         for World of
           Harriet.Db.World.Select_By_Star_System (Context.Star_System)
         loop
            Refs.Append (World.Reference);
         end loop;
      elsif Context.Faction /= Null_Faction_Reference then
         for Star_System of Harriet.Db.Star_System.Scan_By_Name loop
            Refs.Append (Star_System.Reference);
         end loop;
      else
         for Faction of Harriet.Db.Faction.Scan_By_Name loop
            Refs.Append (Faction.Reference);
         end loop;
      end if;

      for Ref of Refs loop
         Process (Ref);
      end loop;
   end Iterate_Contents;

   ----------
   -- Show --
   ----------

   function Show (Context : Context_Type) return String is
      use Harriet.Db;
      Faction_Prompt     : constant String :=
                             (if Context.Faction = Null_Faction_Reference
                              then "/"
                              else Harriet.Factions.Name
                                (Context.Faction) & ":");
      Star_System_Prompt : constant String :=
                             (if Context.Star_System
                              = Null_Star_System_Reference
                              then ""
                              else "/"
                              & Harriet.Star_Systems.Name
                                (Context.Star_System));
      World_Prompt       : constant String :=
                             (if Context.World = Null_World_Reference
                              then ""
                              else "/"
                              & Harriet.Worlds.Name (Context.World));
   begin
      return Faction_Prompt & Star_System_Prompt & World_Prompt;
   end Show;

   --------------
   -- To_Child --
   --------------

   procedure To_Child
     (Context    : in out Context_Type;
      Child_Name : String;
      Success    : out Boolean)
   is
      use Harriet.Db;
   begin
      if Context.Moon /= Null_World_Reference then
         Success := False;
      elsif Context.World /= Null_World_Reference then
         Success := False;
      elsif Context.Star_System /= Null_Star_System_Reference then
         Success := False;
         for World of
           Harriet.Db.World.Select_By_Star_System (Context.Star_System)
         loop
            if World.Name = Child_Name then
               Success := True;
               Context.World := World.Reference;
               exit;
            end if;
         end loop;
      elsif Context.Faction /= Null_Faction_Reference then
         Context.Star_System :=
           Harriet.Db.Star_System.First_Reference_By_Name (Child_Name);
         Success := Context.Star_System /= Null_Star_System_Reference;
      else
         Context.Faction :=
           Harriet.Db.Faction.First_Reference_By_Name (Child_Name);
         Success := Context.Faction /= Null_Faction_Reference;
      end if;
   end To_Child;

   ---------------
   -- To_Parent --
   ---------------

   procedure To_Parent (Context : in out Context_Type) is
      use Harriet.Db;
   begin
      if Context.Faction = Null_Faction_Reference then
         null;
      elsif Context.Star_System = Null_Star_System_Reference then
         Context.Faction := Null_Faction_Reference;
      elsif Context.World = Null_World_Reference then
         Context.Star_System := Null_Star_System_Reference;
      elsif Context.Moon = Null_World_Reference then
         Context.World := Null_World_Reference;
      else
         Context.Moon := Null_World_Reference;
      end if;
   end To_Parent;

end Harriet.Contexts;
