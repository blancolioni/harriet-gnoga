package Harriet.Contexts.Markets is

   function Top_Level_Container return Context_Type;

   function Market_Context
     (Market : Harriet.Db.Market_Reference)
      return Context_Type;

end Harriet.Contexts.Markets;
