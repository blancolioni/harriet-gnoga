with Harriet.Db;
private with Harriet.Db.Commodity_Vectors;

with Harriet.Markets;

with Harriet.UI.Models.Tables;

package Harriet.UI.Models.Market is

   type Root_Market_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with private;

   type Market_Model is access all Root_Market_Model'Class;

   function Create
     (Market : Harriet.Db.Market_Reference)
      return Market_Model;

private

   package Commodity_Row_Vectors is
     new Harriet.Db.Commodity_Vectors
       (Harriet.UI.Models.Tables.Table_Row_Count, 0,
        Harriet.UI.Models.Tables."=");

   type Root_Market_Model is
     new Harriet.UI.Models.Tables.Root_Table_Model with
      record
         Reference         : Harriet.Db.Market_Reference;
         Market_Watcher_Id : Harriet.Markets.Market_Handler_Id;
         Commodity_Row     : Commodity_Row_Vectors.Vector;
      end record;

end Harriet.UI.Models.Market;
