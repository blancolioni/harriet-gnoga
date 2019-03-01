with Harriet.Calendar;

with Harriet.Agents;
with Harriet.Stock;

with Harriet.Configure.Worlds;

with Harriet.Db.Account;
with Harriet.Db.Deposit;
with Harriet.Db.Faction;
with Harriet.Db.Generation;
with Harriet.Db.Market;
with Harriet.Db.Pop;
with Harriet.Db.Pop_Group;
with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.Ship;
with Harriet.Db.World;

package body Harriet.Worlds is

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.World_Sector_Reference,
        Harriet.Db."=");

   procedure Check_Surface
     (World : Harriet.Db.World_Reference);

   --------------------
   -- Add_Population --
   --------------------

   procedure Add_Population
     (Sector  : Harriet.Db.World_Sector_Reference;
      Faction : Harriet.Db.Faction_Reference;
      Group   : Harriet.Db.Pop_Group_Reference;
      Size    : Harriet.Quantities.Quantity_Type;
      Cash    : Harriet.Money.Money_Type)
   is
      use type Harriet.Quantities.Quantity_Type;
      Pop : constant Harriet.Db.Pop.Pop_Type :=
              Harriet.Db.Pop.Get_By_Pop_Group_Sector
                (Group, Sector);
      Commodity : constant Harriet.Db.Commodity_Reference :=
                    Harriet.Db.Pop_Group.Get (Group).Get_Commodity_Reference;
   begin
      if Pop.Has_Element then
         Pop.Set_Size (Pop.Size + Size);
         Pop.Set_Capacity (Pop.Capacity + Size);
         Pop.Set_Transported_Size
           (Pop.Transported_Size + Harriet.Quantities.To_Real (Size));
         Harriet.Agents.Add_Cash (Pop, Cash);
         Harriet.Stock.Add_Stock
           (Pop, Commodity, Size, Cash);
      else
         declare
            Account : constant Harriet.Db.Account_Reference :=
                        Harriet.Db.Account.Create
                          (Harriet.Db.Null_Account_Reference, Cash, Cash);
         begin
            Harriet.Db.Pop.Create
              (Active           => True,
               Scheduled        => False,
               Next_Event       => Harriet.Calendar.Clock,
               Manager          => "default-pop",
               Account          => Account,
               Capacity         => Size,
               Transported_Size => Harriet.Quantities.To_Real (Size),
               Faction          => Faction,
               World            =>
                 Harriet.Db.World_Sector.Get (Sector).World,
               World_Sector     => Sector,
               Installation     => Harriet.Db.Null_Installation_Reference,
               Ship             => Harriet.Db.Null_Ship_Reference,
               Pop_Group        => Group,
               Salary           => Harriet.Money.Zero,
               Size             => Size,
               Happiness        => 1.0);
         end;
         Harriet.Stock.Add_Stock
           (Harriet.Db.Pop.Get_By_Pop_Group_Sector
              (Group, Sector),
            Commodity, Size, Cash);

      end if;
   end Add_Population;

   -----------------
   -- Best_Sector --
   -----------------

   function Best_Sector
     (World : Harriet.Db.World_Reference;
      Score : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Real)
      return Harriet.Db.World_Sector_Reference
   is
      Best_Reference : Harriet.Db.World_Sector_Reference :=
                         Harriet.Db.Null_World_Sector_Reference;
      Best_Score     : Real := Real'First;
   begin
      Check_Surface (World);
      for Sector of
        Harriet.Db.World_Sector.Select_By_World
          (World)
      loop
         declare
            This_Score : constant Real :=
                           Score (Sector);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Reference := Sector.Get_World_Sector_Reference;
            end if;
         end;
      end loop;
      return Best_Reference;
   end Best_Sector;

   -------------------
   -- Check_Surface --
   -------------------

   procedure Check_Surface
     (World : Harriet.Db.World_Reference)
   is
      Is_Gen : constant Harriet.Db.Is_Generated_Reference :=
                 Harriet.Db.World.Get (World).Get_Is_Generated_Reference;
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

   -------------
   -- Climate --
   -------------

   function Climate
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Climate_Reference
   is
   begin
      return Harriet.Db.World.Get (World).Climate;
   end Climate;

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

   -----------------
   -- Find_Sector --
   -----------------

   function Find_Sector
     (World : Harriet.Db.World_Reference;
      Test  : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Boolean)
      return Harriet.Db.World_Sector_Reference
   is
   begin
      for Sector of
        Harriet.Db.World_Sector.Select_By_World
          (World)
      loop
         if Test (Sector) then
            return Sector.Get_World_Sector_Reference;
         end if;
      end loop;
      return Harriet.Db.Null_World_Sector_Reference;
   end Find_Sector;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex
   is
      Rec : constant Harriet.Db.World_Sector.World_Sector_Type :=
              Harriet.Db.World_Sector.Get (Sector);
   begin
      return Sector_Vertex'
        (Rec.X, Rec.Y, Rec.Z);
   end Get_Centre;

   --------------------
   -- Get_Neighbours --
   --------------------

   function Get_Neighbours
     (Sector : Harriet.Db.World_Sector_Reference)
      return World_Sector_Array
   is
      Result : World_Sector_Array (1 .. 20);
      Count  : Natural := 0;
   begin
      for Neighbour of
        Harriet.Db.Sector_Neighbour.Select_By_Sector
          (Harriet.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         declare
            Neighbour_Ref : constant Db.Sector_Reference :=
                              Neighbour.Neighbour;
            Neighbour_Sec : constant Db.World_Sector.World_Sector_Type :=
                              Db.World_Sector.Get_World_Sector
                                (Neighbour_Ref);
            World_Sec_Ref : constant Db.World_Sector_Reference :=
                              Neighbour_Sec.Get_World_Sector_Reference;
         begin
            Result (Count) := World_Sec_Ref;
         end;
      end loop;
      return Result (1 .. Count);
   end Get_Neighbours;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Faction_Reference
   is
   begin
      return Harriet.Db.Faction.Get_Faction
        (Harriet.Db.World_Sector.Get (Sector).Owner).Get_Faction_Reference;
   end Get_Owner;

   ---------------
   -- Get_Ships --
   ---------------

   procedure Get_Ships
     (World : Harriet.Db.World_Reference;
      Ships : out Harriet.Ships.Lists.List)
   is
   begin
      for Ship of Harriet.Db.Ship.Select_By_World (World) loop
         Ships.Append (Harriet.Ships.Get (Ship.Get_Ship_Reference));
      end loop;
   end Get_Ships;

   -----------------
   -- Get_Terrain --
   -----------------

   function Get_Terrain
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Terrain_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Terrain;
   end Get_Terrain;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex_Array
   is
      Count  : Natural := 0;
      Result : Sector_Vertex_Array (1 .. 10);
   begin
      for Vertex of
        Harriet.Db.Sector_Vertex.Select_By_Sector
          (Harriet.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         Result (Count) := (Vertex.X, Vertex.Y, Vertex.Z);
      end loop;
      return Result (1 .. Count);
   end Get_Vertices;

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

   ------------
   -- Market --
   ------------

   function Market
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Market_Reference
   is
   begin
      return Harriet.Db.Market.Get_By_World (World).Get_Market_Reference;
   end Market;

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

   --------------------
   -- Scan_Resources --
   --------------------

   procedure Scan_Resources
     (Sector  : Harriet.Db.World_Sector_Reference;
      Process : not null access
        procedure (Resource : Harriet.Db.Resource_Reference;
                   Accessibility : Unit_Real;
                   Abundance     : Non_Negative_Real))
   is
   begin
      for Deposit of
        Harriet.Db.Deposit.Select_By_World_Sector
          (Sector)
      loop
         Process (Deposit.Resource, Deposit.Accessibility,
                  Deposit.Abundance);
      end loop;
   end Scan_Resources;

   ------------------
   -- Scan_Surface --
   ------------------

   procedure Scan_Surface
     (World   : Harriet.Db.World_Reference;
      Process : not null access
        procedure (Sector : Harriet.Db.World_Sector_Reference))
   is
      List : World_Sector_Lists.List;
   begin
      Check_Surface (World);
      for Sector of Harriet.Db.World_Sector.Select_By_World (World) loop
         List.Append (Sector.Get_World_Sector_Reference);
      end loop;

      for Sector of List loop
         Process (Sector);
      end loop;

   end Scan_Surface;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Sector  : Harriet.Db.World_Sector_Reference;
      Faction : Harriet.Db.Faction_Reference)
   is
   begin
      Harriet.Db.World_Sector.Get (Sector).Set_Owner
        (Harriet.Db.Faction.Get (Faction).Get_Owner_Reference);
   end Set_Owner;

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
