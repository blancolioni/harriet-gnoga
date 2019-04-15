with WL.Heaps;

with Harriet.Money;
with Harriet.Weighted_Random_Choices;

with Harriet.Markets;
with Harriet.Ships;
with Harriet.Worlds;

with Harriet.Db.Market;
with Harriet.Db.Ship;

package body Harriet.Managers.Ships is

   type Ship_Trade_State is (Buying, Travelling, Selling);

   type Ship_Trade_Manager is
     new Root_Ship_Manager with
      record
         Market     : Harriet.Db.Market_Reference;
         State      : Ship_Trade_State := Buying;
         Next_Delay : Duration := Harriet.Calendar.Days (1);
      end record;

   overriding function Next_Sleep_Duration
     (Manager : Ship_Trade_Manager)
      return Duration
   is (Manager.Next_Delay);

   overriding procedure Create_Market_Offers
     (Manager : in out Ship_Trade_Manager);

   function Choose_Destination
     (Manager : Ship_Trade_Manager'Class)
      return Harriet.Db.World_Reference;

   procedure Buy_Trade_Goods
     (Manager  : Ship_Trade_Manager'Class;
      From, To : Harriet.Db.Market_Reference);

   procedure Sell_Trade_Goods
     (Manager  : Ship_Trade_Manager'Class)
   is null;

   procedure Set_Destination
     (Manager     : in out Ship_Trade_Manager'Class;
      Destination : Harriet.Db.World_Reference)
   is null;

   ---------------------
   -- Buy_Trade_Goods --
   ---------------------

   procedure Buy_Trade_Goods
     (Manager  : Ship_Trade_Manager'Class;
      From, To : Harriet.Db.Market_Reference)
   is
      type Trade_Commodity_Record is
         record
            Commodity : Harriet.Db.Commodity_Reference;
            Max_Price : Harriet.Money.Price_Type;
            Quantity  : Harriet.Quantities.Quantity_Type;
         end record;

      package Trade_Queues is
        new WL.Heaps
          (Key_Type     => Non_Negative_Real,
           Element_Type => Trade_Commodity_Record,
           "<"          => "<");

      Queue : Trade_Queues.Heap;
   begin
      for Commodity of Harriet.Commodities.All_Commodities loop
         declare
            use Harriet.Money, Harriet.Quantities;
            Remote_Price : constant Price_Type :=
                             Harriet.Markets.Current_Bid_Price
                               (To, Commodity);
            Remote_Demand : constant Quantity_Type :=
                              Harriet.Markets.Daily_Demand
                                (To, Commodity);
            Max_Local_Price : constant Price_Type :=
                                Adjust_Price (Remote_Price, 0.9);
            Local_Supply    : constant Quantity_Type :=
                              Harriet.Markets.Current_Supply
                                  (From, Commodity);
         begin
            if Local_Supply > Zero
              and then Remote_Demand > Zero
            then

               declare
                  Local_Price : constant Price_Type :=
                                  Harriet.Markets.Current_Bid_Price
                                    (From, Commodity);
               begin
                  Manager.Log
                    (Harriet.Commodities.Local_Name (Commodity)
                     & ": market"
                     & Harriet.Db.To_String (To)
                     & ": "
                     & Show (Remote_Demand) & "/" & Show (Remote_Price)
                     & "; local market "
                     & Show (Local_Supply) & "/" & Show (Max_Local_Price));

                  Queue.Insert (To_Real (Local_Price) / To_Real (Remote_Price),
                                Trade_Commodity_Record'
                                  (Commodity => Commodity,
                                   Max_Price => Max_Local_Price,
                                   Quantity  => Local_Supply));
               end;
            end if;
         end;
      end loop;

      while not Queue.Is_Empty loop
         declare
            Commodity : constant Harriet.Db.Commodity_Reference :=
                          Queue.First_Element.Commodity;
            Quantity  : constant Harriet.Quantities.Quantity_Type :=
                          Queue.First_Element.Quantity;
         begin
            Queue.Delete_First;
            Manager.Place_Bid
              (Commodity => Commodity,
               Quantity  => Quantity);
         end;
      end loop;

   end Buy_Trade_Goods;

   ------------------------
   -- Choose_Destination --
   ------------------------

   function Choose_Destination
     (Manager : Ship_Trade_Manager'Class)
      return Harriet.Db.World_Reference
   is
      package Weighted_Destination is
        new Harriet.Weighted_Random_Choices
          (Harriet.Db.Market_Reference);

      Choices : Weighted_Destination.Weighted_Choice_Set;

      function Score_Market
        (Market : Harriet.Db.Market_Reference)
         return Natural;

      ------------------
      -- Score_Market --
      ------------------

      function Score_Market
        (Market : Harriet.Db.Market_Reference)
         return Natural
      is
         use Harriet.Money, Harriet.Quantities;
         Total_Available : Quantity_Type := Zero;
         Score           : Natural := 0;
      begin

         for Commodity of Harriet.Commodities.All_Commodities loop
            declare
               Remote_Price    : constant Price_Type :=
                                   Harriet.Markets.Current_Bid_Price
                                     (Market, Commodity);
               Remote_Demand   : constant Quantity_Type :=
                                   Harriet.Markets.Daily_Demand
                                     (Market, Commodity);
               Local_Supply    : constant Quantity_Type :=
                                   Harriet.Markets.Current_Supply
                                     (Manager.Market, Commodity);
               Trade_Quantity  : constant Quantity_Type :=
                                   Min (Local_Supply, Remote_Demand);
               Local_Price     : constant Price_Type :=
                                   Harriet.Markets.Current_Bid_Price
                                     (Manager.Market, Commodity);
            begin
               if Trade_Quantity > Zero
                 and then Local_Price < Remote_Price
               then
                  Total_Available := Total_Available + Trade_Quantity;
                  Score := Score
                    + Natural
                    (To_Real (Min (Trade_Quantity, Manager.Cargo_Space))
                     * To_Real (Remote_Price) / To_Real (Local_Price));
               end if;
            end;
         end loop;
         return Score;
      end Score_Market;

   begin
      for Market of
        Harriet.Db.Market.Scan_By_World
      loop
         declare
            use type Harriet.Db.Market_Reference;
            Score : constant Natural :=
                      (if Market.Get_Market_Reference = Manager.Market
                       then 0
                       else Score_Market (Market.Get_Market_Reference));
         begin
            Manager.Log
              (Harriet.Worlds.Name (Market.World)
               & ": score" & Score'Image);
            if Score > 0 then
               Choices.Insert (Market.Get_Market_Reference, Score);
            end if;
         end;
      end loop;

      if not Choices.Is_Empty then
         declare
            Market : constant Harriet.Db.Market_Reference :=
                       Choices.Choose;
            World  : constant Harriet.Db.World_Reference :=
                       Harriet.Db.Market.Get (Market).World;
         begin
            Manager.Log ("setting destination: "
                         & Harriet.Worlds.Name (World));
            return World;
         end;
      else
         Manager.Log ("no good trade destinations");
         return Harriet.Db.Null_World_Reference;
      end if;

   end Choose_Destination;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Ship_Manager)
   is
   begin
      Harriet.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;
   end Create_Market_Offers;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Ship_Trade_Manager)
   is
   begin
      Root_Ship_Manager (Manager).Create_Market_Offers;
      case Manager.State is
         when Buying =>
            declare
               use Harriet.Db;
               Destination : constant World_Reference :=
                               Manager.Choose_Destination;
            begin
               if Destination /= Null_World_Reference then
                  Manager.Buy_Trade_Goods
                    (From => Harriet.Worlds.Market (Manager.Current_World),
                     To   => Harriet.Worlds.Market (Destination));
                  Manager.Set_Destination (Destination);
                  Manager.State := Travelling;
               end if;
            end;

         when Travelling =>
            Manager.State := Selling;

         when Selling =>
            Manager.Sell_Trade_Goods;
--            Manager.State := Buying;
      end case;
   end Create_Market_Offers;

   --------------------------
   -- Create_Trade_Manager --
   --------------------------

   function Create_Trade_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Ship    : constant Harriet.Db.Ship.Ship_Type :=
                  Harriet.Db.Ship.Get_Ship (Managed);
         Manager : Ship_Trade_Manager := Ship_Trade_Manager'
           (Harriet.Managers.Agents.Root_Agent_Manager with
            Ship           => Ship.Get_Ship_Reference,
            State          => <>,
            Next_Delay     => <>,
            Market         => Harriet.Worlds.Market (Ship.World),
            Current_World  => Ship.World,
            Current_System => Ship.Star_System,
            Cargo_Space    =>
              Harriet.Quantities.To_Quantity
                (Harriet.Ships.Design_Cargo_Volume (Ship.Ship_Design)));
   begin
      Manager.Initialize_Agent_Manager (Ship, Ship.World, 10.0);
      return new Ship_Trade_Manager'(Manager);
   end Create_Trade_Manager;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Ship_Manager)
   is
   begin
      null;
   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Ship_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
   begin
      null;
   end Get_Required_Stock;

end Harriet.Managers.Ships;
