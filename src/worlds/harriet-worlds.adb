with Harriet.Configure.Worlds;

with Harriet.Db.Generation;
with Harriet.Db.World;

package body Harriet.Worlds is

   procedure Check_Surface
     (World : Harriet.Db.World_Reference);

   -------------------
   -- Check_Surface --
   -------------------

   procedure Check_Surface
     (World : Harriet.Db.World_Reference)
   is
      Is_Gen : constant Harriet.Db.Is_Generated_Reference :=
                 Harriet.Db.World.Get (World).Reference;
      Gen    : constant Harriet.Db.Generation.Generation_Type :=
                 Harriet.Db.Generation.Get_By_Is_Generated
                   (Is_Gen);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Harriet.Configure.Worlds.Generate_Surface (World);
         if Gen.Has_Element then
            Gen.Set_Ready (True);
         else
            Harriet.Db.Generation.Create (Is_Gen, True);
         end if;
      end if;
   end Check_Surface;

   -----------
   -- Clear --
   -----------

   procedure Clear (Selection : in out World_Selection'Class) is
   begin
      Selection.List.Clear;
   end Clear;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Harriet.Db.World_Reference)
      return Boolean)
   is
      New_List : World_Lists.List;
      Changed  : Boolean := False;
   begin
      for World of Selection.List loop
         if Test (World) then
            New_List.Append (World);
         else
            Changed := True;
         end if;
      end loop;
      if Changed then
         Selection.List := New_List;
      end if;
   end Filter;

   ----------------
   -- Get_Worlds --
   ----------------

   function Get_Worlds
     (Selection : World_Selection'Class)
      return World_Array
   is
      Index : Natural := 0;
   begin
      return Arr : World_Array (1 .. Natural (Selection.List.Length)) do
         for World of Selection.List loop
            Index := Index + 1;
            Arr (Index) := World;
         end loop;
      end return;
   end Get_Worlds;

   ------------------
   -- Habitability --
   ------------------

   function Habitability
     (World : Harriet.Db.World_Reference)
      return Unit_Real
   is
   begin
      Check_Surface (World);
      return Harriet.Db.World.Get (World).Habitability;
   end Habitability;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Harriet.Db.World_Reference)
   is
   begin
      Selection.List.Append (World);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Selection : World_Selection'Class) return Boolean is
   begin
      return Selection.List.Is_Empty;
   end Is_Empty;

   --------------------
   -- Is_Terrestrial --
   --------------------

   function Is_Terrestrial
     (World : Harriet.Db.World_Reference)
      return Boolean
   is
      use all type Harriet.Db.World_Category;
   begin
      return Harriet.Db.World.Get (World).Category in
        Terrestrial .. Super_Terrestrial;
   end Is_Terrestrial;

   ----------
   -- Mass --
   ----------

   function Mass
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.World.Get (World).Mass;
   end Mass;

   ----------
   -- Name --
   ----------

   function Name
     (World : Harriet.Db.World_Reference)
      return String
   is
   begin
      return Harriet.Db.World.Get (World).Name;
   end Name;

   ------------
   -- Radius --
   ------------

   function Radius
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.World.Get (World).Radius;
   end Radius;

   -----------------
   -- Star_System --
   -----------------

   function Star_System
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.World.Get (World).Star_System;
   end Star_System;

end Harriet.Worlds;
