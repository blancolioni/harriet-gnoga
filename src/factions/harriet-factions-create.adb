with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Sets;

with Harriet.Calendar;
with Harriet.Configure;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Real_Images;
with Harriet.Roman_Images;

with Harriet.Ships;
with Harriet.Stock;
with Harriet.Star_Systems;
with Harriet.Terrain;
with Harriet.Worlds;

with Harriet.Db.Account;
with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Worker;
with Harriet.Db.Faction;
with Harriet.Db.Generated_Resource;
with Harriet.Db.Installation;
with Harriet.Db.Market;
with Harriet.Db.Pop_Group;
with Harriet.Db.Pop_Group_Needs;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Generator;
with Harriet.Db.Script;
with Harriet.Db.Script_Line;
with Harriet.Db.Ship_Design;
with Harriet.Db.Star_System_Distance;
with Harriet.Db.World;
with Harriet.Db.World_Sector;
with Harriet.Db.User;

with Harriet.Db.Commodity;
with Harriet.Db.Consumer_Good;
with Harriet.Db.Industrial_Good;

package body Harriet.Factions.Create is

   Log_Faction_Creation : constant Boolean := False;

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
     (Faction     : Harriet.Db.Faction_Reference;
      World       : Harriet.Db.World_Reference;
      Hub_Sector  : Harriet.Db.World_Sector_Reference;
      Everywhere  : Boolean;
      Config      : Tropos.Configuration);

   function Choose_Facility
     (Sector            : Harriet.Db.World_Sector_Reference;
      Min_Accessibility : Unit_Real)
      return Harriet.Db.Facility_Reference;

   ---------------------
   -- Choose_Facility --
   ---------------------

   function Choose_Facility
     (Sector            : Harriet.Db.World_Sector_Reference;
      Min_Accessibility : Unit_Real)
      return Harriet.Db.Facility_Reference
   is
      use Harriet.Db;

      Best_Resource      : Harriet.Db.Resource_Reference :=
                             Harriet.Db.Null_Resource_Reference;
      Best_Accessibility : Unit_Real := 0.0;
      Best_Abundance     : Non_Negative_Real := 0.0;

      procedure Check_Resource
        (Resource      : Harriet.Db.Resource_Reference;
         Accessibility : Unit_Real;
         Abundance     : Non_Negative_Real);

      --------------------
      -- Check_Resource --
      --------------------

      procedure Check_Resource
        (Resource      : Harriet.Db.Resource_Reference;
         Accessibility : Unit_Real;
         Abundance     : Non_Negative_Real)
      is
      begin
         if Accessibility > Best_Accessibility then
            Best_Resource := Resource;
            Best_Accessibility := Accessibility;
            Best_Abundance := Abundance;
         end if;
      end Check_Resource;

      Facility : Harriet.Db.Facility_Reference;

   begin
      Harriet.Worlds.Scan_Resources
        (Sector, Check_Resource'Access);

      if Log_Faction_Creation then
         Ada.Text_IO.Put
           ("sector"
            & Harriet.Db.To_String (Sector)
            & " with terrain "
            & Harriet.Terrain.Name
              (Harriet.Worlds.Get_Terrain (Sector))
            & ": ");
      end if;

      if Best_Resource = Harriet.Db.Null_Resource_Reference
        or else Best_Accessibility < Min_Accessibility
      then
         if Log_Faction_Creation then
            Ada.Text_IO.Put_Line
              ("no available resources");
         end if;

         Facility :=
           Harriet.Db.Facility.Get_Reference_By_Tag ("light-factory");
      else

         if Log_Faction_Creation then
            Ada.Text_IO.Put_Line
              ("best resource: "
               & Harriet.Db.Resource.Get (Best_Resource).Tag
               & " accessibility "
               & Harriet.Real_Images.Approximate_Image
                 (Best_Accessibility * 100.0)
               & "% abundance "
               & Harriet.Real_Images.Approximate_Image
                 (Best_Abundance * 100.0));
         end if;

         declare
            use Harriet.Db.Generated_Resource;
            Generated : constant Generated_Resource_Type :=
                          First_By_Resource (Best_Resource);
         begin
            if Generated.Has_Element then
               Facility :=
                 Harriet.Db.Resource_Generator.Get
                   (Generated.Resource_Generator)
                 .Get_Facility_Reference;
            else
               Ada.Text_IO.Put_Line ("no resource generators");
               Facility :=
                 Harriet.Db.Facility.Get_Reference_By_Tag ("light-factory");
            end if;
         end;
      end if;
      if Log_Faction_Creation then
         Ada.Text_IO.Put_Line
           ("choosing: "
            & Harriet.Db.Facility.Get (Facility).Tag);
      end if;
      return Facility;
   end Choose_Facility;

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
                     Harriet.Configure.Configure_Money
                       (Setup, "cash", 1000.0);
         Account : constant Harriet.Db.Account_Reference :=
                     Harriet.Db.Account.Create
                       (Guarantor  => Harriet.Db.Null_Account_Reference,
                        Start_Cash => Cash,
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
                        Active        => True,
                        Scheduled     => False,
                        Next_Event    => Harriet.Calendar.Clock,
                        Manager       => "",
                        Account       => Account,
                        Capacity      => Harriet.Quantities.Zero,
                        Red           => Color.Red,
                        Green         => Color.Green,
                        Blue          => Color.Blue,
                        User          => User,
                        Capital_System =>
                          Harriet.Worlds.Star_System (Capital),
                        Capital_World  => Capital);

         Script     : constant Harriet.Db.Script_Reference :=
                        Harriet.Db.Script.Create ("rc", User);
         Line_Index : Natural := 0;

      begin

         if not Setup.Contains ("init-script") then
            Ada.Text_IO.Put_Line
              ("warning: no initial script in " & Setup.Config_Name);
         end if;

         for Command of Setup.Child ("init-script") loop
            Line_Index := Line_Index + 1;
            Harriet.Db.Script_Line.Create
              (Script => Script,
               Index  => Line_Index,
               Line   => Command.Config_Name);
         end loop;

         Harriet.Db.World.Get (Capital).Set_Owner
           (Harriet.Db.Faction.Get (Faction).Get_Owner_Reference);

         Harriet.Db.Market.Create
           (World => Capital);

         Create_Initial_Ships
           (Faction, Capital, Setup.Child ("ships"));

         Create_Initial_Installations
           (Faction, Capital, Sector,
            Setup.Get ("capital"),
            Setup.Child ("installations"));

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
                       (Login         => Config.Config_Name,
                        Password      => "",
                        Administrator => False);
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
     (Faction     : Harriet.Db.Faction_Reference;
      World       : Harriet.Db.World_Reference;
      Hub_Sector  : Harriet.Db.World_Sector_Reference;
      Everywhere  : Boolean;
      Config      : Tropos.Configuration)
   is
      Owner : constant Harriet.Db.Owner_Reference :=
                Harriet.Db.Faction.Get (Faction).Get_Owner_Reference;
      Hub   : Harriet.Db.Installation_Reference :=
                Harriet.Db.Null_Installation_Reference;

      procedure Create_Sector_Installation
        (Sector   : Harriet.Db.World_Sector_Reference);

      function Create_Installation
        (Facility : Harriet.Db.Facility_Reference;
         Sector   : Harriet.Db.World_Sector_Reference;
         Cash     : Harriet.Money.Money_Type;
         Manager  : String)
         return Harriet.Db.Installation_Reference;

      -------------------------
      -- Create_Installation --
      -------------------------

      function Create_Installation
        (Facility : Harriet.Db.Facility_Reference;
         Sector   : Harriet.Db.World_Sector_Reference;
         Cash     : Harriet.Money.Money_Type;
         Manager  : String)
         return Harriet.Db.Installation_Reference
      is
         use type Harriet.Db.Installation_Reference;

         Capacity : constant Harriet.Quantities.Quantity_Type :=
                      Harriet.Db.Facility.Get (Facility).Capacity;
         Account  : constant Harriet.Db.Account_Reference :=
                      Harriet.Db.Account.Create
                        (Harriet.Db.Null_Account_Reference,
                         Cash, Cash);

         Ref : constant Harriet.Db.Installation_Reference :=
                      Harriet.Db.Installation.Create
                        (Owner        => Owner,
                         Capacity     => Capacity,
                         Account      => Account,
                         World        => World,
                         World_Sector => Sector,
                         Facility     => Facility,
                         Production   => Harriet.Db.Null_Commodity_Reference,
                         Active       => True,
                         Scheduled    => False,
                         Next_Event   =>
                           Harriet.Calendar.Delay_Days
                             (Harriet.Random.Unit_Random),
                         Manager      => Manager);
      begin
         if Hub = Harriet.Db.Null_Installation_Reference then
            Hub := Ref;
         end if;

         Harriet.Worlds.Set_Owner (Sector, Faction);

         for Employee of
           Harriet.Db.Facility_Worker.Select_By_Facility
             (Facility)
         loop
            declare
               use Harriet.Money;
            begin
               Harriet.Worlds.Add_Population
                 (Sector  => Hub_Sector,
                  Faction => Faction,
                  Group   => Employee.Pop_Group,
                  Size    => Employee.Quantity,
                  Cash    =>
                    Total
                      (Harriet.Db.Pop_Group.Get (Employee.Pop_Group).Salary,
                       Employee.Quantity));
               for Needs of
                 Harriet.Db.Pop_Group_Needs.Select_By_Pop_Group
                   (Employee.Pop_Group)
               loop
                  Harriet.Stock.Add_Initial_Stock
                    (Harriet.Db.Installation.Get (Hub).Get_Has_Stock_Reference,
                     Needs.Commodity,
                     Harriet.Quantities.Scale (Employee.Quantity, 17.0));
               end loop;

            end;
         end loop;
         return Ref;
      end Create_Installation;

      --------------------------------
      -- Create_Sector_Installation --
      --------------------------------

      procedure Create_Sector_Installation
        (Sector   : Harriet.Db.World_Sector_Reference)
      is
      begin
         Harriet.Worlds.Set_Owner (Sector, Faction);

         declare
            Min_Access : constant Unit_Real :=
                           (if Everywhere
                            then 0.5
                            else 0.1);
            Ref : constant Harriet.Db.Installation_Reference :=
                    Create_Installation
                      (Facility => Choose_Facility (Sector, Min_Access),
                       Sector   => Sector,
                       Cash     => Harriet.Money.To_Money (1.0E6),
                       Manager  => "default-installation");
         begin
            pragma Unreferenced (Ref);
         end;
      end Create_Sector_Installation;

   begin

      Harriet.Worlds.Set_Owner (Hub_Sector, Faction);

      for Installation_Config of Config loop
         declare
            use Harriet.Money;
            Facility : constant Harriet.Db.Facility_Reference :=
                         Harriet.Db.Facility.Get_Reference_By_Tag
                           (Installation_Config.Config_Name);
            Cash     : constant Money_Type :=
                         Harriet.Configure.Configure_Money
                           (Installation_Config, "cash", 1.0E6);
            Manager  : constant String :=
                         Installation_Config.Get
                           ("manager", "default-installation");
            Ref : constant Harriet.Db.Installation_Reference :=
                         Create_Installation
                           (Facility, Hub_Sector, Cash, Manager);
            Stock    : constant Harriet.Db.Has_Stock_Reference :=
                         Harriet.Db.Installation.Get (Ref)
                         .Get_Has_Stock_Reference;
         begin
            for Item of Installation_Config.Child ("stock") loop
               declare
                  use Harriet.Quantities;
                  Tag : constant String := Item.Config_Name;
                  Qty : constant Quantity_Type :=
                          To_Quantity (Real (Float'(Item.Value)));
               begin
                  if Tag = "consumer-goods" then
                     for Commodity of
                       Harriet.Db.Consumer_Good.Select_By_Available (True)
                     loop
                        Harriet.Stock.Add_Initial_Stock
                          (Stock, Commodity.Get_Commodity_Reference, Qty);
                     end loop;
                  elsif Tag = "industrial-goods" then
                     for Commodity of
                       Harriet.Db.Industrial_Good.Select_By_Available (True)
                     loop
                        Harriet.Stock.Add_Initial_Stock
                          (Stock, Commodity.Get_Commodity_Reference, Qty);
                     end loop;
                  elsif Tag = "resource" then
                     for Commodity of
                       Harriet.Db.Resource.Select_By_Available (True)
                     loop
                        Harriet.Stock.Add_Initial_Stock
                          (Stock, Commodity.Get_Commodity_Reference, Qty);
                     end loop;
                  else
                     declare
                        use Harriet.Db;
                        Commodity : constant Commodity_Reference :=
                                      Harriet.Db.Commodity.Get_Reference_By_Tag
                                        (Tag);
                     begin
                        if Commodity = Null_Commodity_Reference then
                           raise Constraint_Error with
                             "in configuration for "
                             & Installation_Config.Config_Name
                             & ": no such commodity: "
                             & Tag;
                        end if;

                        Harriet.Stock.Add_Initial_Stock
                          (Stock, Commodity, Qty);
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      if Everywhere then

         declare
            procedure Create (Sector : Harriet.Db.World_Sector_Reference);

            ------------
            -- Create --
            ------------

            procedure Create (Sector : Harriet.Db.World_Sector_Reference) is
               use type Harriet.Db.World_Sector_Reference;
            begin
               if Sector /= Hub_Sector then
                  Create_Sector_Installation (Sector);
               end if;
            end Create;

         begin
            Harriet.Worlds.Scan_Surface (World, Create'Access);
         end;

      else
         for Neighbour of Harriet.Worlds.Get_Neighbours (Hub_Sector) loop
            Create_Sector_Installation (Neighbour);
         end loop;
      end if;

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
                     Manager =>
                       Harriet.Db.Ship_Design.Get (Design).Default_Manager,
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
                   Harriet.Worlds.Get_Neighbours
                     (Sector.Get_World_Sector_Reference);
         begin
            for N of Ns loop
               for Deposit of
                 Harriet.Db.Deposit.Select_By_World_Sector (N)
               loop
                  Score :=
                    Real'Max
                      (Score,
                         Deposit.Accessibility
--                      + Deposit.Abundance / 1.0e6 * Deposit.Accessibility
                       * (1.0 - Harriet.Terrain.Hazard
                         (Harriet.Worlds.Get_Terrain (N))));
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
              Db.Star_System_Distance
                .Select_Star_System_Range_Bounded_By_Distance
                  (Star_System, 0.0, 99.0)
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
