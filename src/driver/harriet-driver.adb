with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;
with WL.Random.Names;

with Gnoga.Application.Multi_Connect;

with Tropos.Reader;
with Tropos.Writer;

with Harriet.Options;
with Harriet.Paths;

with Harriet.Color;
with Harriet.Random;

with Harriet.UI.Gnoga_UI;

with Harriet.Calendar;
with Harriet.Configure;
with Harriet.Configure.Scenarios;
with Harriet.Factions.Create;
with Harriet.Logging;
with Harriet.Logs;
with Harriet.Repl;
with Harriet.Sessions;

with Harriet.Commands.Loader;
with Harriet.Managers.Loader;
with Harriet.Managers.Execution;

with Harriet.Updates.Control;

with Harriet.Db.Database;

with Harriet.Db.Faction;
with Harriet.Db.User;

procedure Harriet.Driver is
   Name_Generator : WL.Random.Names.Name_Generator;

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean;

   ----------------------------
   -- Have_Required_Argument --
   ----------------------------

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean
   is
   begin
      if Argument_Value = "" then
         Ada.Text_IO.Put_Line
           ("missing required argument: --" & Argument_Name);
         return False;
      else
         return True;
      end if;
   end Have_Required_Argument;

   Database_Open   : Boolean := False;
   Updates_Running : Boolean := False;

begin

   if not Ada.Directories.Exists ("options.txt") then
      Ada.Directories.Copy_File
        (Source_Name => Harriet.Paths.Config_File ("default-options.txt"),
         Target_Name => "options.txt");
   end if;

   WL.Command_Line.Load_Defaults ("options.txt");

   WL.Localisation.Read_Localisation
     (Harriet.Paths.Config_File
        ("localisation/" & Harriet.Options.Language & ".txt"));

   WL.Random.Names.Load_Lexicon
     (Name_Generator,
      Harriet.Paths.Config_File ("totro-vowels.txt"),
      Harriet.Paths.Config_File ("totro-consonants.txt"));

   if Harriet.Options.Randomise then
      WL.Random.Randomise;
   end if;

   if Harriet.Options.Create then
      Harriet.Db.Database.Create;
      Database_Open := True;

      Harriet.Configure.Initialize_Database;

      Harriet.Configure.Scenarios.Load_Scenario
        (Scenario_Name  => Harriet.Options.Scenario,
         Name_Generator => Name_Generator);

      Harriet.Db.Database.Close;
      Database_Open := False;
      return;
   end if;

   if Harriet.Options.Add_Faction then

      if Harriet.Options.Faction_Names_File /= "" then
         if not Have_Required_Argument
           ("faction-colors-file", Harriet.Options.Faction_Colors_File)
         then
            return;
         end if;

         Harriet.Random.Reset;
         WL.Random.Randomise;

         Harriet.Db.Database.Open;
         Database_Open := True;

         declare
            Name_Config : constant Tropos.Configuration :=
                               Tropos.Reader.Read_Config
                                 (Harriet.Paths.Config_File
                                    (Harriet.Options.Faction_Names_File));
            Color_Config   : constant Tropos.Configuration :=
                               Tropos.Reader.Read_Config
                                 (Harriet.Paths.Config_File
                                    (Harriet.Options.Faction_Colors_File));
            Scenario_Name  : constant String :=
                               (if Harriet.Options.Scenario = ""
                                then "default"
                                else Harriet.Options.Scenario);
            Faction_Setup  : constant String :=
                               Harriet.Options.Faction_Setup_Path;
            Setup_Path     : constant String :=
                               Harriet.Configure.Scenario_File
                                 (Scenario_Name  => Scenario_Name,
                                  Directory_Name => "factions",
                                  File_Name      =>
                                    (if Faction_Setup = ""
                                     then "default"
                                     else Faction_Setup)
                                  & ".faction");
            Faction_Config : Tropos.Configuration :=
                               Tropos.New_Config ("factions");
            Position : Tropos.Cursor := Color_Config.First;
         begin

            for Config of Name_Config loop
               declare
                  New_Config : Tropos.Configuration := Config;
               begin
                  New_Config.Add
                    ("color", Tropos.Element (Position).Config_Name);
                  Faction_Config.Add (New_Config);
                  Tropos.Next (Position);
               end;
            end loop;
            Tropos.Writer.Write_Config (Faction_Config, "factions.config");

            Harriet.Factions.Create.Create_Factions
              (Faction_Config => Faction_Config,
               Setup_Config   => Tropos.Reader.Read_Config (Setup_Path));
         end;

         Harriet.Db.Database.Close;
         Database_Open := False;

         return;
      end if;

      if not Have_Required_Argument
        ("account-name", Harriet.Options.Account_Name)
        or else not Have_Required_Argument
          ("faction-name", Harriet.Options.Faction_Name)
      then
         return;
      end if;

      Harriet.Db.Database.Open;
      Database_Open := True;

      declare
         use Harriet.Db;
         User           : constant Harriet.Db.User_Reference :=
                            Harriet.Db.User.Get_Reference_By_Login
                              (Login => Harriet.Options.Account_Name);
         Scenario_Name  : constant String :=
                            (if Harriet.Options.Scenario = ""
                             then "default"
                             else Harriet.Options.Scenario);
         Faction_Name   : constant String :=
                            Harriet.Options.Faction_Name;
         Faction_Adj    : constant String :=
                            Harriet.Options.Faction_Adjective;
         Faction_Plural : constant String :=
                            Harriet.Options.Faction_Plural_Name;
         Faction_Color  : constant String :=
                            Harriet.Options.Faction_Color;
         Faction_Setup  : constant String :=
                            Harriet.Options.Faction_Setup_Path;
         Faction        : constant Harriet.Db.Faction_Reference :=
                            Harriet.Db.Faction.First_Reference_By_Name
                              (Name => Faction_Name);
         Setup_Path     : constant String :=
                              Harriet.Configure.Scenario_File
                                (Scenario_Name  => Scenario_Name,
                                 Directory_Name => "factions",
                                 File_Name      =>
                                   (if Faction_Setup = ""
                                    then "default"
                                    else Faction_Setup)
                                 & ".faction");
      begin
         if User = Null_User_Reference then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Harriet.Options.Account_Name
               & ": unknown user name");
            Harriet.Db.Database.Close;
            return;
         end if;

         if Faction /= Null_Faction_Reference then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Faction_Name
               & ": faction already exists");
            Harriet.Db.Database.Close;
            return;
         end if;

         if not Ada.Directories.Exists (Setup_Path) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Cannot find setup file " & Setup_Path);
            Harriet.Db.Database.Close;
            return;
         end if;

         if Faction_Setup = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Using default faction setup " & Setup_Path);
         end if;

         declare
            Color : constant Harriet.Color.Harriet_Color :=
                      (if Faction_Color /= ""
                       then Harriet.Color.From_String (Faction_Color)
                       else Harriet.Color.Harriet_Color'
                         (Red   => Harriet.Random.Unit_Random,
                          Green => Harriet.Random.Unit_Random,
                          Blue  => Harriet.Random.Unit_Random,
                          Alpha => 1.0));
            New_Faction : constant Faction_Reference :=
                            Harriet.Factions.Create.Create_Faction
                              (User        => User,
                               Name        => Faction_Name,
                               Adjective   => Faction_Adj,
                               Plural_Name => Faction_Plural,
                               Color       => Color,
                               Setup       =>
                                 Tropos.Reader.Read_Config (Setup_Path));
         begin
            if New_Faction = Null_Faction_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to create faction");
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Faction created successfully");
            end if;
         end;

         Harriet.Db.Database.Close;
         Database_Open := False;
         return;
      end;

   end if;

   Harriet.Managers.Loader.Register_Managers;
   Harriet.Commands.Loader.Load_Commands;

   Harriet.Logging.Start_Logging;

   Ada.Text_IO.Put_Line ("opening database ...");

   Harriet.Db.Database.Open;
   Database_Open := True;

   Harriet.Managers.Execution.Load_Managers;

   Ada.Text_IO.Put_Line ("starting server ...");

   Harriet.Calendar.Load_Clock;

   Ada.Text_IO.Put_Line
     ("Start date: " & Harriet.Calendar.Image (Harriet.Calendar.Clock));

   Harriet.Updates.Control.Start_Updates;
   Updates_Running := True;

   if Harriet.Options.Command_Line then
      Ada.Text_IO.Put_Line ("H A R R I E T");
      if Harriet.Options.Timed_Run = 0 then
         declare
            User    : constant Harriet.Db.User_Reference :=
                        Harriet.Db.User.Get_Reference_By_Login ("root");
            Session : Harriet.Sessions.Harriet_Session :=
                        Harriet.Sessions.New_Repl_Session (User);
         begin
            Harriet.Repl.Execute (Session);
            Harriet.Sessions.End_Session (Session);
         end;
      else
         delay Duration (Harriet.Options.Timed_Run);
      end if;
   else
      Gnoga.Application.Title ("Harriet");

      Gnoga.Application.HTML_On_Close ("Application disconnected.");

      Gnoga.Application.Multi_Connect.Initialize
        (Port    => 8080,
         Boot    => "harriet.html",
         Verbose => True);

      Gnoga.Application.Multi_Connect.On_Connect_Handler
        (Event => Harriet.UI.Gnoga_UI.On_Connect_Default'Unrestricted_Access,
         Path  => "default");

      Gnoga.Application.Multi_Connect.Message_Loop;
   end if;

   Updates_Running := False;
   Harriet.Updates.Control.Stop_Updates;

   Ada.Text_IO.Put_Line
     ("Stop date: " & Harriet.Calendar.Image (Harriet.Calendar.Clock));

   Ada.Text_IO.Put_Line ("Closing database");
   Harriet.Db.Database.Close;
   Database_Open := False;

   Harriet.Logs.Flush_Logs (True);

   Ada.Text_IO.Put_Line ("exit");

   if Harriet.Options.Detailed_Logging then
      Harriet.Logging.Stop_Logging;
   end if;

exception

   when others =>
      if Updates_Running then
         Harriet.Updates.Control.Stop_Updates;
      end if;
      if Database_Open then
         Harriet.Db.Database.Close;
      end if;
      Harriet.Logs.Flush_Logs (True);
      Harriet.Logging.Stop_Logging;
      raise;

end Harriet.Driver;
