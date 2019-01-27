with Tropos;

with Harriet.Money;
with Harriet.Quantities;

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

   function Configure_Money
     (Config  : Tropos.Configuration;
      Field   : String;
      Default : Non_Negative_Real)
      return Harriet.Money.Money_Type;

   function Configure_Price
     (Config : Tropos.Configuration;
      Field  : String)
      return Harriet.Money.Price_Type;

   function Configure_Quantity
     (Config : Tropos.Configuration;
      Field  : String)
      return Harriet.Quantities.Quantity_Type;

   procedure Configure_Association
     (Config : Tropos.Configuration;
      Field  : String;
      Configure : not null access
        procedure (Left, Right : String));

   function Get_Real
     (Config  : Tropos.Configuration;
      Field   : String;
      Default : Real := 0.0)
      return Real;

   function Real_Value
     (Config  : Tropos.Configuration)
      return Real;

end Harriet.Configure;
