with Tropos;

package Harriet.Configure is

   procedure Initialize_Database;

   function Scenario_Directory
     (Scenario_Name  : String;
      Directory_Name : String)
      return String;

   function Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return String;

   procedure Scan_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Path : String));

   procedure Load_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Config : Tropos.Configuration));

   function Load_Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return Tropos.Configuration;

end Harriet.Configure;
