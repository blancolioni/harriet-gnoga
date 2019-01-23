private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Harriet.Db;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.Star_System is

   type Root_Star_System_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   function Star_System
     (Model : Root_Star_System_Model'Class)
      return Harriet.Db.Star_System_Reference;

   function World_Count
     (Model : Root_Star_System_Model'Class)
      return Natural;

   function Name
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return String;

   function Category
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Harriet.Db.World_Category;

   function Climate
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Harriet.Db.Climate_Category;

   function Radius
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  relative to Earth radius

   function Orbit
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  relative to Earth orbit; i.e. in AU

   function Current_Longitude
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  in degrees

   function Find_World
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Harriet.Db.World_Reference;

   function Find_World_Index
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Natural;

   type Star_System_Model is
     access all Root_Star_System_Model'Class;

   function Create
     (Star_System : Harriet.Db.Star_System_Reference)
      return Star_System_Model;

private

   type World_Record is
      record
         Reference       : Harriet.Db.World_Reference;
         Name            : Ada.Strings.Unbounded.Unbounded_String;
         Category        : Harriet.Db.World_Category;
         Climate         : Harriet.Db.Climate_Category;
         Radius          : Non_Negative_Real;
         Orbit           : Non_Negative_Real;
         Year            : Non_Negative_Real;
         Start_Longitude : Non_Negative_Real;
      end record;

   package World_Vectors is
     new Ada.Containers.Vectors (Positive, World_Record);

   type Root_Star_System_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         Star_System : Harriet.Db.Star_System_Reference;
         Vector      : World_Vectors.Vector;
      end record;

   function Star_System
     (Model : Root_Star_System_Model'Class)
      return Harriet.Db.Star_System_Reference
   is (Model.Star_System);

   function World_Count
     (Model : Root_Star_System_Model'Class)
      return Natural
   is (Model.Vector.Last_Index);

   function Name
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return String
   is (Ada.Strings.Unbounded.To_String (Model.Vector.Element (Index).Name));

   function Category
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Harriet.Db.World_Category
   is (Model.Vector.Element (Index).Category);

   function Climate
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Harriet.Db.Climate_Category
   is (Model.Vector.Element (Index).Climate);

   function Radius
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Vector.Element (Index).Radius);

   function Orbit
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Vector.Element (Index).Orbit);

end Harriet.UI.Models.Star_System;
