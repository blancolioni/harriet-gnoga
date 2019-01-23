with Harriet.UI.Models;
with Harriet.UI.Gnoga_UI.Views;

generic
   type Base_View_Type is
     abstract new Harriet.UI.Gnoga_UI.Views.Root_View_Type with private;
   type View_Model_Type is
     new Harriet.UI.Models.Root_Harriet_Model with private;
package Harriet.UI.Gnoga_UI.Generic_Views is

   type View_Type is
     abstract new Base_View_Type
     and Harriet.UI.Models.Model_Watcher
   with private;

   overriding function Title
     (View : View_Type)
      return String;

   overriding procedure Model_Changed
     (View : in out View_Type)
   is null;

   function Model
     (View : View_Type'Class)
      return access View_Model_Type'Class;

   procedure Set_Model
     (View : in out View_Type'Class;
      Model : not null access View_Model_Type'Class);

private

   type Model_Access is access all View_Model_Type'Class;

   type View_Type is
     abstract new Base_View_Type
     and Harriet.UI.Models.Model_Watcher with
      record
         Model : Model_Access;
      end record;

   overriding function Title
     (View : View_Type)
      return String
   is (View.Model.Title);

   function Model
     (View : View_Type'Class)
      return access View_Model_Type'Class
   is (View.Model);

end Harriet.UI.Gnoga_UI.Generic_Views;
