with Ada.Text_IO;

with Harriet.Quantities;
with Harriet.Random;

with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Generated_Resource;
with Harriet.Db.Installation;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Generator;

package body Harriet.Managers.Installations is

   type Resource_Generator_Manager is
     new Root_Installation_Manager with
      record
         Rgen : Harriet.Db.Resource_Generator_Reference;
      end record;

   overriding procedure Activate
     (Manager : not null access Resource_Generator_Manager);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Resource_Generator_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);
      Gen          : constant Harriet.Db.Resource_Generator
        .Resource_Generator_Type :=
          Harriet.Db.Resource_Generator.Get
            (Manager.Rgen);
   begin
      Ada.Text_IO.Put_Line
        (Gen.Tag & ": activating");

      for Deposit of
        Harriet.Db.Deposit.Select_By_World_Sector
          (Installation.World_Sector)
      loop
         declare
            Resource : constant Harriet.Db.Resource_Reference :=
                         Deposit.Resource;
         begin
            if Harriet.Db.Generated_Resource.Is_Generated_Resource
              (Gen.Reference, Resource)
            then
               declare
                  use Harriet.Quantities;
                  Quantity : constant Quantity_Type :=
                               To_Quantity
                                 (Deposit.Accessibility * 20.0
                                  * (Harriet.Random.Unit_Random + 0.5));
               begin
                  Ada.Text_IO.Put_Line
                    ("generated " & Show (Quantity)
                     & " " & Harriet.Db.Resource.Get (Resource).Tag);
               end;
            else
               Ada.Text_IO.Put_Line
                 ("cannot generate "
                  & Harriet.Db.Resource.Get (Resource).Tag);
            end if;
         end;
      end loop;

      Manager.Set_Next_Update_Delay
        (Harriet.Calendar.Days (1));

   end Activate;

   ------------
   -- Create --
   ------------

   function Create
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation
                         (Managed);
      Facility     : constant Harriet.Db.Facility.Facility_Type :=
                       Harriet.Db.Facility.Get
                         (Installation.Facility);
   begin
      case Facility.Top_Record is
         when Harriet.Db.R_Resource_Generator =>
            return new Resource_Generator_Manager'
              (Installation => Installation.Reference,
               Rgen         =>
                  Harriet.Db.Resource_Generator.Get_Resource_Generator
                    (Installation.Facility).Reference,
               others       => <>);
         when others =>
            Ada.Text_IO.Put_Line
              ("warning: "
               & "no manager for facility "
               & Facility.Tag);
            return null;
      end case;
   end Create;

end Harriet.Managers.Installations;
