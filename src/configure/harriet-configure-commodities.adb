with Tropos.Reader;

with Harriet.Money;
with Harriet.Quantities;

with Harriet.Db.Commodity;
with Harriet.Db.Commodity_Vectors;

with Harriet.Db.Recipe;
with Harriet.Db.Recipe_Input;

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

   procedure Configure_Components
     (Config : Tropos.Configuration);

   procedure Configure_Prices;

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
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "commodities"),
         Extension => "commodity",
         Configure => Configure_Components'Access);
      Configure_Prices;
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
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Fissile then
            Harriet.Db.Fissile.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Fuel then
            Harriet.Db.Fuel.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Gas then
            Harriet.Db.Gas.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Liquid then
            Harriet.Db.Liquid.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Metal then
            Harriet.Db.Metal.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Mineral then
            Harriet.Db.Mineral.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Organic then
            Harriet.Db.Organic.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "building-modules" then
         if Commercial then
            Harriet.Db.Commercial_Module.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Habitation then
            Harriet.Db.Habitation_Module.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Industrial then
            Harriet.Db.Industrial_Module.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Military then
            Harriet.Db.Military_Module.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Structural then
            Harriet.Db.Structural.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "consumer-goods" then
         if Clothing then
            Harriet.Db.Clothing_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Drink then
            Harriet.Db.Drink_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Food then
            Harriet.Db.Food_Commodity.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Intoxicant then
            Harriet.Db.Intoxicant.Create
              (Quality      => Quality,
               Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      elsif Class = "industrial-goods" then
         if Alloy then
            Harriet.Db.Alloy.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Ceramic then
            Harriet.Db.Ceramic.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Electronics then
            Harriet.Db.Electronics.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         elsif Plastic then
            Harriet.Db.Plastics.Create
              (Available    => True,
               Initial_Price => Price,
               Mass         => Real (Mass),
               Tag          => Tag);
         end if;
      end if;
   end Configure_Commodity;

   procedure Configure_Components
     (Config : Tropos.Configuration)
   is
      use Harriet.Db;
      Commodity : constant Commodity_Reference :=
                    Harriet.Db.Commodity.Get_Reference_By_Tag
                      (Config.Config_Name);
   begin
      if Config.Contains ("component")
        and then Commodity /= Null_Commodity_Reference
      then
         declare
            Component_Config : constant Tropos.Configuration :=
                                 Config.Child ("component");
            Commodity_Config : constant Tropos.Configuration :=
                                 Component_Config.Child ("type");
            Quantity_Config  : constant Tropos.Configuration :=
                                 Component_Config.Child ("qty");
            Recipe           : constant Harriet.Db.Recipe_Reference :=
                                 Harriet.Db.Recipe.Create (Commodity);
         begin
            for Commodity_Tag of Commodity_Config loop
               declare
                  Input     : constant Commodity_Reference :=
                                Harriet.Db.Commodity.Get_Reference_By_Tag
                                  (Commodity_Tag.Value);
                  Quantity  : constant Harriet.Quantities.Quantity_Type :=
                                Harriet.Quantities.To_Quantity
                                  (Real
                                     (Float'(Quantity_Config.Get
                                      (Commodity_Tag.Config_Name))));
               begin
                  Harriet.Db.Recipe_Input.Create
                    (Recipe    => Recipe,
                     Commodity => Input,
                     Quantity  => Quantity);
               end;
            end loop;
         end;
      end if;
   end Configure_Components;

   ----------------------
   -- Configure_Prices --
   ----------------------

   procedure Configure_Prices is

      use Harriet.Money;
      use Harriet.Quantities;

      package Commodity_Price_Vectors is
        new Harriet.Db.Commodity_Vectors
          (Harriet.Money.Price_Type, Harriet.Money.Zero, Harriet.Money."=");

      Initial_Price : Commodity_Price_Vectors.Vector;

      procedure Check_Prices
        (Success : out Boolean);

      procedure Update_Price
        (Commodity : Harriet.Db.Commodity.Commodity_Type;
         Success   : in out Boolean);

      ------------------
      -- Check_Prices --
      ------------------

      procedure Check_Prices
        (Success : out Boolean)
      is
      begin
         Success := True;

         for Commodity of Harriet.Db.Commodity.Scan_By_Top_Record loop
            if Initial_Price.Element (Commodity.Reference) = Zero then
               Update_Price (Commodity, Success);
            end if;
         end loop;

      end Check_Prices;

      ------------------
      -- Update_Price --
      ------------------

      procedure Update_Price
        (Commodity : Harriet.Db.Commodity.Commodity_Type;
         Success   : in out Boolean)
      is
         Total_Cost : Money_Type := Zero;
         Recipe     : constant Harriet.Db.Recipe_Reference :=
                    Harriet.Db.Recipe.First_Reference_By_Commodity
                      (Commodity.Reference);
      begin
         for Input of Harriet.Db.Recipe_Input.Select_By_Recipe (Recipe) loop
            declare
               Price : constant Price_Type :=
                         Initial_Price.Element (Input.Commodity);
               Quantity : constant Quantity_Type :=
                            Input.Quantity;
            begin
               if Price = Zero then
                  Success := False;
                  return;
               end if;
               Total_Cost := Total_Cost + Total (Price, Quantity);
            end;
         end loop;

         Initial_Price.Replace_Element
           (Commodity.Reference,
            Price (Adjust (Total_Cost, 1.5), Unit));

      end Update_Price;

      Finished : Boolean := False;

   begin
      for Commodity of Harriet.Db.Commodity.Scan_By_Top_Record loop
         Initial_Price.Replace_Element
           (Commodity.Reference, Commodity.Initial_Price);
      end loop;

      while not Finished loop
         Check_Prices (Finished);
      end loop;

      for Commodity of Harriet.Db.Commodity.Scan_By_Top_Record loop
         Commodity.Set_Initial_Price
           (Initial_Price.Element
              (Commodity.Reference));
      end loop;

   end Configure_Prices;

end Harriet.Configure.Commodities;
