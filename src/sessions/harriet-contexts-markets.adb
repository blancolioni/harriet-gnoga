with Harriet.Contexts.Containers;

with Harriet.Worlds;

with Harriet.Db.Market;

package body Harriet.Contexts.Markets is

   type Market_Context_Type is
     new Root_Context_Type with
      record
         Market : Harriet.Db.Market_Reference;
      end record;

   overriding function Is_Valid
     (Context : Market_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : Market_Context_Type;
      Children : in out Context_List'Class);

   overriding function Name
     (Context : Market_Context_Type)
      return String
   is (Harriet.Worlds.Name
       (Harriet.Db.Market.Get (Context.Market).World));

   procedure Scan_Markets
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type));

   package Market_Containers is
     new Harriet.Contexts.Containers
       (Container_Name    => "markets",
        Context_Reference => Boolean,
        Iterate_Children  => Scan_Markets);

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Market_Context_Type;
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
     (Context : Market_Context_Type)
      return Boolean
   is
      use type Harriet.Db.Market_Reference;
   begin
      return Context.Market /= Harriet.Db.Null_Market_Reference;
   end Is_Valid;

   --------------------
   -- Market_Context --
   --------------------

   function Market_Context
     (Market : Harriet.Db.Market_Reference)
      return Context_Type
   is
   begin
      return Market_Context_Type'
        (Market => Market);
   end Market_Context;

   ------------------
   -- Scan_Markets --
   ------------------

   procedure Scan_Markets
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type))
   is
   begin
      for Market of
        Harriet.Db.Market.Scan_By_Top_Record
      loop
         Process (Market_Context (Market.Reference));
      end loop;
   end Scan_Markets;

   -------------------------
   -- Top_Level_Container --
   -------------------------

   function Top_Level_Container return Context_Type is
   begin
      return Market_Containers.Container_Context (True);
   end Top_Level_Container;

end Harriet.Contexts.Markets;
