with Ada.Directories;
with Ada.Text_IO;

with WL.Random;

with Tropos.Reader;

with Harriet.Configure.Resources;

with Harriet.Options;
with Harriet.Paths;

with Harriet.Calendar;

with Harriet.Db.User;
with Harriet.Db.Calendar;

package body Harriet.Configure is

   function Initial_Root_Password return String;

   ---------------------------
   -- Initial_Root_Password --
   ---------------------------

   function Initial_Root_Password return String is
   begin
      if Harriet.Options.Generate_Root_Password then
         return Password : String (1 .. 12) do
            for Ch of Password loop
               Ch :=
                 Character'Val (WL.Random.Random_Number (65, 90));
            end loop;
            Ada.Text_IO.Put_Line ("root password: " & Password);
         end return;
      else
         return "";
      end if;
   end Initial_Root_Password;

   -------------------------
   -- Initialize_Database --
   -------------------------

   procedure Initialize_Database is
   begin
      Harriet.Db.User.Create ("root", Initial_Root_Password);
      Harriet.Db.Calendar.Create
        (Clock => Harriet.Calendar.Clock);
      Harriet.Configure.Resources.Configure_Atmosphere_Components
        (Tropos.Reader.Read_Config
           (Harriet.Paths.Config_File
                ("star-systems/atmosphere.txt")));
   end Initialize_Database;

   ------------------------
   -- Load_Scenario_File --
   ------------------------

   function Load_Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return Tropos.Configuration
   is
   begin
      return Tropos.Reader.Read_Config
        (Scenario_File (Scenario_Name, Directory_Name, File_Name));
   end Load_Scenario_File;

   -------------------------
   -- Load_Scenario_Files --
   -------------------------

   procedure Load_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Config : Tropos.Configuration))
   is
      procedure Call_Process (Path : String);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Path : String) is
      begin
         Process (Tropos.Reader.Read_Config (Path));
      end Call_Process;

   begin
      Scan_Scenario_Files
        (Scenario_Name, Directory_Name, File_Class_Name,
         Call_Process'Access);
   end Load_Scenario_Files;

   -------------------------
   -- Scan_Scenario_Files --
   -------------------------

   procedure Scan_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Path : String))
   is
      procedure Call_Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Process (Ada.Directories.Full_Name (Directory_Entry));
      end Call_Process;

   begin
      Ada.Directories.Search
        (Directory =>
           Scenario_Directory (Scenario_Name, Directory_Name),
         Pattern   => "*." & File_Class_Name,
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Call_Process'Access);
   end Scan_Scenario_Files;

   ------------------------
   -- Scenario_Directory --
   ------------------------

   function Scenario_Directory
     (Scenario_Name  : String;
      Directory_Name : String)
      return String
   is
      Scenario_Path : constant String :=
                        Harriet.Paths.Config_File
                          ("scenarios/" & Scenario_Name
                           & "/" & Directory_Name);
      Regular_Path  : constant String :=
                        Harriet.Paths.Config_File (Directory_Name);
   begin
      if Ada.Directories.Exists (Scenario_Path) then
         return Scenario_Path;
      elsif Ada.Directories.Exists (Regular_Path) then
         return Regular_Path;
      else
         raise Constraint_Error with
         Scenario_Name
           & ": cannot find scenario directory " & Directory_Name;
      end if;
   end Scenario_Directory;

   -------------------
   -- Scenario_File --
   -------------------

   function Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return String

   is
      Scenario_Path : constant String :=
                        Harriet.Paths.Config_File
                          ("scenarios/" & Scenario_Name
                           & "/" & Directory_Name
                           & "/" & File_Name);
      Regular_Path  : constant String :=
                        Harriet.Paths.Config_File
                          (Directory_Name
                           & "/" & File_Name);
   begin
      if Ada.Directories.Exists (Scenario_Path) then
         return Scenario_Path;
      elsif Ada.Directories.Exists (Regular_Path) then
         return Regular_Path;
      else
         raise Constraint_Error with
         Scenario_Name
           & ": cannot find scenario directory " & Directory_Name;
      end if;
   end Scenario_File;

end Harriet.Configure;
