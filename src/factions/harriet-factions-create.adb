with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Sets;

with Harriet.Money;
with Harriet.Quantities;
with Harriet.Roman_Images;

with Harriet.Ships;
with Harriet.Star_Systems;
with Harriet.Terrain;
with Harriet.Worlds;

with Harriet.Db.Account;
with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Faction;
with Harriet.Db.Installation;
with Harriet.Db.Ship_Design;
with Harriet.Db.Star_System_Distance;
with Harriet.Db.World_Sector;
with Harriet.Db.User;

package body Harriet.Factions.Create is

   function Find_Homeworld
     return Harriet.Db.World_Reference;

   function Find_Home_Sector
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.World_Sector_Reference;

   procedure Create_Initial_Ships
     (Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Config  : Tropos.Configuration);

   procedure Create_Initial_Installations
     (Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Sector  : Harriet.Db.World_Sector_Reference;
      Config  : Tropos.Configuration);

   --------------------
   -- Create_Faction --
   --------------------

   function Create_Faction
     (User        : Harriet.Db.User_Reference;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Harriet.Color.Harriet_Color;
      Setup       : Tropos.Configuration)
      return Harriet.Db.Faction_Reference
   is
      use Harriet.Db;
      Capital : constant Harriet.Db.World_Reference :=
                  Find_Homeworld;
   begin
      if Capital = Null_World_Reference then
         return Null_Faction_Reference;
      end if;

      declare
         Cash    : constant Harriet.Money.Money_Type :=
                     Harriet.Money.To_Money
                       (Real (Float'(Setup.Get ("cash"))));
         Account : constant Harriet.Db.Account_Reference :=
                     Harriet.Db.Account.Create
                       (Start_Cash => Cash,
                        Cash       => Cash);
         Sector  : constant Harriet.Db.World_Sector_Reference :=
                     Find_Home_Sector (Capital);
         Faction : constant Harriet.Db.Faction_Reference :=
                     Harriet.Db.Faction.Create
                       (Name          => Name,
                        Adjective     =>
                          (if Adjective = "" then Name else Adjective),
                        Plural_Name   =>
                          (if Plural_Name = "" then Name else Plural_Name),
                        Account       => Account,
                        Capacity      => Harriet.Quantities.Zero,
                        Red           => Color.Red,
                        Green         => Color.Green,
                        Blue          => Color.Blue,
                        User          => User,
                        Capital_System =>
                          Harriet.Worlds.Star_System (Capital),
                        Capital_World => Capital);
      begin

         Create_Initial_Ships
           (Faction, Capital, Setup.Child ("ships"));

         Create_Initial_Installations
           (Faction, Capital, Sector, Setup.Child ("installations"));

         return Faction;
      end;
   end Create_Faction;

   ---------------------
   -- Create_Factions --
   ---------------------

   procedure Create_Factions
     (Faction_Config : Tropos.Configuration;
      Setup_Config   : Tropos.Configuration)
   is
   begin
      for Config of Faction_Config loop
         Ada.Text_IO.Put_Line
           ("new faction: " & Config.Get ("name"));
         declare
            use type Harriet.Db.Faction_Reference;
            User : constant Harriet.Db.User_Reference :=
                     Harriet.Db.User.Create
                       (Login    => Config.Config_Name,
                        Password => "");
            Faction : constant Harriet.Db.Faction_Reference :=
                        Create_Faction
                          (User        => User,
                           Name        => Config.Get ("name"),
                           Adjective   =>
                             Config.Get ("adjective", Config.Get ("name")),
                           Plural_Name =>
                             Config.Get ("plural", Config.Get ("name")),
                           Color       =>
                             Harriet.Color.From_String
                               (Config.Get ("color", "#ff0000")),
                           Setup       => Setup_Config);
         begin
            if Faction = Harriet.Db.Null_Faction_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "failed to create faction");
            end if;
         end;
      end loop;
   end Create_Factions;

   ----------------------------------
   -- Create_Initial_Installations --
   ----------------------------------

   procedure Create_Initial_Installations
     (Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Sector  : Harriet.Db.World_Sector_Reference;
      Config  : Tropos.Configuration)
   is
      pragma Unreferenced (World);
   begin
      for Installation_Config of Config loop
         declare
            Facility : constant Harriet.Db.Facility_Reference :=
                         Harriet.Db.Facility.Get_Reference_By_Tag
                           (Installation_Config.Config_Name);
         begin
            Harriet.Db.Installation.Create
              (Owner        =>
                 Harriet.Db.Faction.Get (Faction).Reference,
               World_Sector => Sector,
               Facility     => Facility);
            Harriet.Worlds.Set_Owner (Sector, Faction);
         end;
      end loop;
   end Create_Initial_Installations;

   --------------------------
   -- Create_Initial_Ships --
   --------------------------

   procedure Create_Initial_Ships
     (Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Config  : Tropos.Configuration)
   is
   begin
      for Design_Config of Config loop
         declare
            use type Harriet.Db.Ship_Design_Reference;
            Design_Name : constant String := Design_Config.Config_Name;
            Design      : constant Harriet.Db.Ship_Design_Reference :=
                            Harriet.Db.Ship_Design.First_Reference_By_Name
                              (Design_Name);
            Count       : constant Natural :=
                            Design_Config.Value;

            function Create_Ship_Name
              (Design_Name : String;
               Index       : Positive)
                  return String;

            function Create_Ship_Name
              (Design_Name : String;
               Index       : Positive)
                  return String
            is
               use Ada.Characters.Handling;
               First  : Boolean := True;
               Result : String := Design_Name;
            begin
               for Ch of Result loop
                  if First then
                     Ch := To_Upper (Ch);
                     First := False;
                  elsif Is_Letter (Ch) then
                     Ch := To_Lower (Ch);
                  elsif Ch in '-' | '_' then
                     Ch := ' ';
                     First := True;
                  end if;
               end loop;

               if Count = 1 then
                  return Result;
               else
                  return Result & " "
                    & Harriet.Roman_Images.Roman_Image (Index);
               end if;
            end Create_Ship_Name;

         begin
            if Design = Harriet.Db.Null_Ship_Design_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "error: design '" & Design_Config.Config_Name
                  & "' does not exist");
            else
               for I in 1 .. Count loop
                  Harriet.Ships.Create_Ship
                    (Owner  => Faction,
                     World  => World,
                     Design => Design,
                     Name   => Create_Ship_Name (Design_Name, I));
               end loop;
            end if;
         end;
      end loop;
   end Create_Initial_Ships;

   ----------------------
   -- Find_Home_Sector --
   ----------------------

   function Find_Home_Sector
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.World_Sector_Reference
   is
      function Score_Sector
        (Sector : Harriet.Db.World_Sector.World_Sector_Type)
         return Real;

      ------------------
      -- Score_Sector --
      ------------------

      function Score_Sector
        (Sector : Harriet.Db.World_Sector.World_Sector_Type)
         return Real
      is
         Score : Real := 0.0;
      begin
         if Harriet.Terrain.Is_Water (Sector.Terrain) then
            return Real'First;
         end if;

         declare
            Ns : constant Harriet.Worlds.World_Sector_Array :=
                   Harriet.Worlds.Get_Neighbours (Sector.Reference);
         begin
            for N of Ns loop
               for Deposit of
                 Harriet.Db.Deposit.Select_By_World_Sector (N)
               loop
                  Score := Score
                    + Deposit.Abundance / 1.0e6 * Deposit.Accessibility
                    * (1.0 - Harriet.Terrain.Hazard
                       (Harriet.Worlds.Get_Terrain (N)));
               end loop;
            end loop;

            return Score * (1.0 - Harriet.Terrain.Hazard (Sector.Terrain));
         end;
      end Score_Sector;

   begin
      return Harriet.Worlds.Best_Sector (World, Score_Sector'Access);
   end Find_Home_Sector;

   --------------------
   -- Find_Homeworld --
   --------------------

   function Find_Homeworld
     return Harriet.Db.World_Reference
   is

      package Star_System_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Harriet.Db.Star_System_Reference,
           Harriet.Db."=");

      Queue : Star_System_Lists.List;
      Checked : WL.String_Sets.Set;

      function Check_World
        (World : Harriet.Db.World_Reference)
         return Boolean;

      -----------------
      -- Check_World --
      -----------------

      function Check_World
        (World : Harriet.Db.World_Reference)
         return Boolean
      is
      begin
         return Harriet.Worlds.Habitability (World) > 0.7;
      end Check_World;

   begin

      Queue.Append (Harriet.Star_Systems.First);
      Checked.Insert (Harriet.Star_Systems.Name (Harriet.Star_Systems.First));

      while not Queue.Is_Empty loop
         declare
            use Harriet.Star_Systems;
            Star_System : constant Harriet.Db.Star_System_Reference :=
                            Queue.First_Element;
         begin
            Queue.Delete_First;

            if not Claimed (Star_System) then
               declare
                  Selection : constant Harriet.Worlds.World_Selection :=
                                Harriet.Star_Systems.Terrestrial_Worlds
                                  (Star_System);
               begin
                  if not Selection.Is_Empty then
                     for W of Selection.Get_Worlds loop
                        if Check_World (W) then
                           Claim (Star_System);
                           return W;
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            for Neighbour of
              Db.Star_System_Distance.Select_Bounded_By_Star_System_Range
                (Star_System, 0.0, Star_System, 99.0)
            loop
               declare
                  Neighbour_Name : constant String :=
                                     Name (Neighbour.To);
               begin
                  if not Checked.Contains (Neighbour_Name) then
                     Checked.Insert (Neighbour_Name);
                     Queue.Append (Neighbour.To);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Harriet.Db.Null_World_Reference;
   end Find_Homeworld;

end Harriet.Factions.Create;
