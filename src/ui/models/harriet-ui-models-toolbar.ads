package Harriet.UI.Models.Toolbar is

   type Root_Toolbar_Model is
     new Root_Harriet_Model with private;

   type Toolbar_Model is access all Root_Toolbar_Model'Class;

   function Create_Toolbar_Model
      return Toolbar_Model;

private

   type Root_Toolbar_Model is
     new Root_Harriet_Model with null record;

   overriding function Title
     (Model : Root_Toolbar_Model)
      return String
   is ("toolbar");

end Harriet.UI.Models.Toolbar;
