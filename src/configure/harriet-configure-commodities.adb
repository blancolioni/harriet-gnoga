with Tropos.Reader;

with Harriet.Money;

with Harriet.Db.Alloy;
with Harriet.Db.Ceramic;
with Harriet.Db.Clothing_Commodity;
with Harriet.Db.Commercial_Module;
with Harriet.Db.Drink_Commodity;
with Harriet.Db.Electronics;
with Harriet.Db.Fissile;
with Harriet.Db.Fuel;
with Harriet.Db.Food_Commodity;
with Harriet.Db.Gas;
with Harriet.Db.Habitation_Module;
with Harriet.Db.Industrial_Module;
with Harriet.Db.Intoxicant;
with Harriet.Db.Liquid;
with Harriet.Db.Metal;
with Harriet.Db.Military_Module;
with Harriet.Db.Mineral;
with Harriet.Db.Organic;
with Harriet.Db.Plastics;
with Harriet.Db.Structural;

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
      Tag         : constant String := Config.Config_Name;
      Class       : constant String := Config.Get ("class");
      Quality     : constant Natural := Config.Get ("quality", 0);
      Price       : constant Harriet.Money.Price_Type :=
                      Harriet.Money.To_Price
                        (Real (Float'(Config.Get ("npc-price", 0.0))));
      Mass        : constant Float := Config.Get ("mass");
      Alloy       : constant Boolean := Config.Get ("alloy");
      Ceramic     : constant Boolean := Config.Get ("ceramic");
      Clothing    : constant Boolean := Config.Get ("clothing");
      Commercial  : constant Boolean := Config.Get ("commercial");
      Drink       : constant Boolean := Config.Get ("drink");
      Electronics : constant Boolean := Config.Get ("electronic");
      Fissile     : constant Boolean := Config.Get ("fissile");
      Food        : constant Boolean := Config.Get ("food");
      Fuel        : constant Boolean := Config.Get ("fuel");
      Gas         : constant Boolean := Config.Get ("gas");
      Habitation  : constant Boolean := Config.Get ("habitation");
      Industrial  : constant Boolean := Config.Get ("industrial");
      Intoxicant  : constant Boolean := Config.Get ("intoxicant");
      Liquid      : constant Boolean := Config.Get ("liquid");
      Metal       : constant Boolean := Config.Get ("metal");
      Military    : constant Boolean := Config.Get ("military");
      Mineral     : constant Boolean := Config.Get ("mineral");
      Organic     : constant Boolean := Config.Get ("organic");
      Plastic     : constant Boolean := Config.Get ("plastic");
      Structural  : constant Boolean := Config.Get ("structural");
   begin
      if Class = "resource" then
         if Fissile then
            Harriet.Db.Fissile.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Fissile then
            Harriet.Db.Fissile.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Fuel then
            Harriet.Db.Fuel.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Gas then
            Harriet.Db.Gas.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Liquid then
            Harriet.Db.Liquid.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Metal then
            Harriet.Db.Metal.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Mineral then
            Harriet.Db.Mineral.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Organic then
            Harriet.Db.Organic.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "building-modules" then
         if Commercial then
            Harriet.Db.Commercial_Module.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Habitation then
            Harriet.Db.Habitation_Module.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Industrial then
            Harriet.Db.Industrial_Module.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Military then
            Harriet.Db.Military_Module.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Structural then
            Harriet.Db.Structural.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "consumer-goods" then
         if Clothing then
            Harriet.Db.Clothing_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Drink then
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
         elsif Intoxicant then
            Harriet.Db.Intoxicant.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "industrial-goods" then
         if Alloy then
            Harriet.Db.Alloy.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Ceramic then
            Harriet.Db.Ceramic.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Electronics then
            Harriet.Db.Electronics.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Plastic then
            Harriet.Db.Plastics.Create
              (Available    => True,
               Initial_Cost => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      end if;
   end Configure_Commodity;

end Harriet.Configure.Commodities;
