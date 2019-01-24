with Tropos.Reader;

with Harriet.Money;

with Harriet.Db.Drink_Commodity;
with Harriet.Db.Food_Commodity;
with Harriet.Db.Organic;

package body Harriet.Configure.Commodities is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "commodities"),
         Extension => "commodity",
         Configure => Configure_Commodity'Access);
   end Configure_Commodities;

   -------------------------
   -- Configure_Commodity --
   -------------------------

   procedure Configure_Commodity
     (Config : Tropos.Configuration)
   is
      Tag     : constant String := Config.Config_Name;
      Class   : constant String := Config.Get ("class");
      Quality : constant Natural := Config.Get ("quality", 0);
      Price   : constant Harriet.Money.Price_Type :=
                  Harriet.Money.To_Price
                    (Real (Float'(Config.Get ("npc-price", 0.0))));
      Mass    : constant Float := Config.Get ("mass");
      Drink   : constant Boolean := Config.Get ("drink");
      Food    : constant Boolean := Config.Get ("food");
      Organic : constant Boolean := Config.Get ("organic");
   begin
      if Class = "resource" then
         if Organic then
            Harriet.Db.Organic.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "consumer-goods" then
         if Drink then
            Harriet.Db.Drink_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Food then
            Harriet.Db.Food_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      end if;
   end Configure_Commodity;

end Harriet.Configure.Commodities;
