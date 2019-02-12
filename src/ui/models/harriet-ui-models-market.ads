with Harriet.Db;

with Harriet.Sessions;
with Harriet.Signals;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.Market is

   type Root_Market_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   type Market_Model is access all Root_Market_Model'Class;

   function Create
     (Session : Harriet.Sessions.Harriet_Session;
      Market : Harriet.Db.Market_Reference)
      return Market_Model;

private

   type Root_Market_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         Reference : Harriet.Db.Market_Reference;
         Clock_Handler_Id : Harriet.Signals.Handler_Id;
      end record;

end Harriet.UI.Models.Market;
