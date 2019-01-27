with Harriet.Money;

with Harriet.Db.Pop_Group;

package body Harriet.Configure.Pop_Groups is

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Pop_Group --
   -------------------------

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration)
   is
      Price : constant Harriet.Money.Price_Type :=
                Configure_Price (Config, "salary");
   begin
      Harriet.Db.Pop_Group.Create
        (Available        => True,
         Initial_Price    => Price,
         Mass             => 1.0,
         Tag              => Config.Config_Name,
         Consumer_Quality => Config.Child ("quality").Get ("consumer"),
         Service_Quality  => Config.Child ("quality").Get ("service"),
         Salary           => Price);
   end Configure_Pop_Group;

   --------------------------
   -- Configure_Pop_Groups --
   --------------------------

   procedure Configure_Pop_Groups
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "poptypes",
         File_Class_Name => "poptype",
         Process         => Configure_Pop_Group'Access);
   end Configure_Pop_Groups;

end Harriet.Configure.Pop_Groups;
