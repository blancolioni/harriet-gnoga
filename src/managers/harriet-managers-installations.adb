with Ada.Text_IO;

with Harriet.Data_Series;
with Harriet.Logging;
with Harriet.Money;
with Harriet.Options;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Real_Images;
with Harriet.Stock;
with Harriet.Weighted_Random_Choices;

with Harriet.Worlds;

with Harriet.Db.Consumer_Good;
with Harriet.Db.Deposit;
with Harriet.Db.Facility;
with Harriet.Db.Facility_Worker;
with Harriet.Db.Factory;
with Harriet.Db.Generated_Resource;
with Harriet.Db.Installation;
with Harriet.Db.Pop_Group;
with Harriet.Db.Produced_Commodity;
with Harriet.Db.Recipe;
with Harriet.Db.Recipe_Input;
with Harriet.Db.Resource;
with Harriet.Db.Resource_Generator;

package body Harriet.Managers.Installations is

   type Resource_Generator_Manager is
     new Root_Installation_Manager with
      record
         Rgen : Harriet.Db.Resource_Generator_Reference;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Resource_Generator_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager);

   type Factory_Manager is
     new Root_Installation_Manager with
      record
         Factory    : Harriet.Db.Factory_Reference;
         Production : Harriet.Db.Commodity_Reference :=
                        Harriet.Db.Null_Commodity_Reference;
         Recipe     : Harriet.Db.Recipe_Reference :=
                        Harriet.Db.Null_Recipe_Reference;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Factory_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Factory_Manager)
   is null;

   procedure Choose_Recipe
     (Manager : in out Factory_Manager'Class);

   type Hub_Manager is
     new Root_Installation_Manager with
      record
         Day_Tick  : Natural := 0;
         Log_State : Boolean := False;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Hub_Manager);

   -------------------
   -- Choose_Recipe --
   -------------------

   procedure Choose_Recipe
     (Manager : in out Factory_Manager'Class)
   is

      package Recipe_Choices is
        new Harriet.Weighted_Random_Choices
          (Harriet.Db.Recipe_Reference);

      Choices : Recipe_Choices.Weighted_Choice_Set;

      function Score_Production
        (Commodity : Harriet.Db.Commodity_Reference;
         Recipe    : Harriet.Db.Recipe_Reference)
         return Natural;

      ----------------------
      -- Score_Production --
      ----------------------

      function Score_Production
        (Commodity : Harriet.Db.Commodity_Reference;
         Recipe    : Harriet.Db.Recipe_Reference)
         return Natural
      is
         use Harriet.Money, Harriet.Quantities;
         Input_Cost  : Money_Type := Zero;
         Worker_Cost : Money_Type := Zero;
         Capacity    : constant Quantity_Type :=
                         Harriet.Db.Facility.Get (Manager.Facility).Capacity;
         Value : constant Money_Type :=
                         Total (Manager.Current_Market_Bid_Price (Commodity),
                                Capacity);
      begin
         for Input of Harriet.Db.Recipe_Input.Select_By_Recipe (Recipe) loop
            Input_Cost := Input_Cost
              + Total
              (Manager.Current_Market_Ask_Price (Input.Commodity),
               Input.Quantity);
         end loop;

         for Worker of
           Harriet.Db.Facility_Worker.Select_By_Facility
             (Manager.Facility)
         loop
            Worker_Cost := Worker_Cost
              + Total
              (Manager.Current_Market_Ask_Price
                 (Harriet.Db.Pop_Group.Get (Worker.Pop_Group)
                  .Get_Commodity_Reference),
               Worker.Quantity);
         end loop;

         declare
            Input_Price : constant Price_Type :=
                            Price (Input_Cost, Unit);
            Total_Cost  : constant Money_Type :=
                            Total (Input_Price, Capacity) + Worker_Cost;
            Profit      : constant Real :=
                            (To_Real (Value) / To_Real (Total_Cost) - 1.0);
            Score       : constant Natural :=
                            (if Profit > 0.0
                             then Natural (Profit * 1000.0)
                             else 0);
         begin
            Harriet.Logging.Log
              (Actor    => Harriet.Db.To_String (Manager.Installation),
               Location => Harriet.Worlds.Name (Manager.World),
               Category => "production",
               Message  =>
                 Harriet.Commodities.Local_Name (Commodity)
               & ": input price " & Show (Input_Price)
               & "; total input cost " & Show (Total (Input_Price, Capacity))
               & "; worker cost " & Show (Worker_Cost)
               & "; expected earnings " & Show (Value)
               & (if Profit <= 0.0 then ""
                 else "; profit margin "
                 & Harriet.Real_Images.Approximate_Image (Profit * 100.0)
                 & "%")
               & "; score" & Score'Image);
            return Score;
         end;
      end Score_Production;

   begin
      for Produced of
        Harriet.Db.Produced_Commodity.Select_By_Factory
          (Manager.Factory)
      loop
         for Recipe of
           Harriet.Db.Recipe.Select_By_Commodity
             (Produced.Commodity)
         loop
            Choices.Insert
              (Recipe.Get_Recipe_Reference,
               Score_Production
                 (Produced.Commodity,
                  Recipe.Get_Recipe_Reference));
         end loop;
      end loop;

      if not Choices.Is_Empty then
         declare
            Choice : constant Harriet.Db.Recipe_Reference :=
                       Choices.Choose;
         begin
            Harriet.Logging.Log
              (Actor    => Harriet.Db.To_String (Manager.Installation),
               Location => Harriet.Worlds.Name (Manager.World),
               Category => "production",
               Message  =>
                 "producing: "
               & Harriet.Commodities.Local_Name
                 (Harriet.Db.Recipe.Get (Choice).Commodity));
            Manager.Recipe := Choice;
            Manager.Production :=
              Harriet.Db.Recipe.Get (Choice).Commodity;
            Harriet.Db.Installation.Get (Manager.Installation).Set_Production
              (Manager.Production);
         end;
      end if;
   end Choose_Recipe;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation
                         (Managed);
      Facility     : constant Harriet.Db.Facility.Facility_Type :=
                       Harriet.Db.Facility.Get
                         (Installation.Facility);
   begin
      case Facility.Top_Record is
         when Harriet.Db.R_Resource_Generator =>
            Harriet.Logging.Log
              (Actor    => Facility.Tag,
               Location => Harriet.Worlds.Name (Installation.World),
               Category => "manager",
               Message  => "starting");

            declare
               Rgen : constant Harriet.Db.Resource_Generator_Reference :=
                        Harriet.Db.Resource_Generator.Get_Resource_Generator
                          (Installation.Facility)
                          .Get_Resource_Generator_Reference;
               Manager : Resource_Generator_Manager :=
                           (Harriet.Managers.Agents.Root_Agent_Manager with
                            Installation =>
                              Installation.Get_Installation_Reference,
                            Facility        => Installation.Facility,
                            Rgen            => Rgen);
            begin
               Manager.Initialize_Agent_Manager
                 (Installation, Installation.World);
               return new Resource_Generator_Manager'(Manager);
            end;

         when Harriet.Db.R_Factory =>
            Harriet.Logging.Log
              (Actor    => Facility.Tag,
               Location => Harriet.Worlds.Name (Installation.World),
               Category => "manager",
               Message  => "starting");

            declare
               Factory : constant Harriet.Db.Factory_Reference :=
                           Harriet.Db.Factory.Get_Factory
                             (Installation.Facility)
                             .Get_Factory_Reference;
               Manager : Factory_Manager :=
                           (Harriet.Managers.Agents.Root_Agent_Manager with
                            Installation    =>
                              Installation.Get_Installation_Reference,
                            Facility        => Installation.Facility,
                            Factory         => Factory,
                            Production      => Installation.Production,
                            Recipe          =>
                              Harriet.Db.Recipe.First_Reference_By_Commodity
                                (Installation.Production));
            begin
               Manager.Initialize_Agent_Manager
                 (Installation, Installation.World);
               return new Factory_Manager'(Manager);
            end;

         when others =>
            Ada.Text_IO.Put_Line
              ("warning: "
               & "no manager for facility "
               & Facility.Tag);
            return null;
      end case;
   end Create_Default_Manager;

   ------------------------
   -- Create_Hub_Manager --
   ------------------------

   function Create_Hub_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get_Installation
                         (Managed);
      Manager      : Hub_Manager :=
                       Hub_Manager'
                         (Harriet.Managers.Agents.Root_Agent_Manager with
                          Installation =>
                            Installation.Get_Installation_Reference,
                          Facility     => Installation.Facility,
                          Log_State    => Harriet.Options.Log_Trade_Offers,
                          Day_Tick     => 0);
   begin
      Manager.Initialize_Agent_Manager
        (Installation, Installation.World);
      return new Hub_Manager'(Manager);
   end Create_Hub_Manager;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Resource_Generator_Manager)
   is
   begin
      Harriet.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;

      for Gen of
        Harriet.Db.Generated_Resource.Select_By_Resource_Generator
          (Manager.Rgen)
      loop
         declare
            use Harriet.Quantities;
            Commodity : constant Harriet.Db.Commodity_Reference :=
                          Harriet.Db.Resource.Get (Gen.Resource)
                          .Get_Commodity_Reference;
            Quantity  : constant Quantity_Type :=
                          Manager.Current_Stock (Commodity);
         begin
            if Quantity > Zero then
               Manager.Place_Ask
                 (Commodity => Commodity,
                  Quantity  => Quantity,
                  Price     => Manager.Current_Market_Bid_Price (Commodity));
            end if;
         end;
      end loop;

   end Create_Market_Offers;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Factory_Manager)
   is
      use Harriet.Db;

      procedure Add_Ask
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type);

      -------------
      -- Add_Ask --
      -------------

      procedure Add_Ask
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type)
      is
      begin
         if Item /= Manager.Production
           and then not Harriet.Db.Recipe_Input.Is_Recipe_Input
             (Manager.Recipe, Item)
         then
            declare
               use type Harriet.Money.Money_Type;
               Price : constant Harriet.Money.Price_Type :=
                         Manager.Current_Market_Bid_Price (Item);
            begin
               if Harriet.Money.Total (Price, Quantity) > Value then
                  Manager.Place_Ask
                    (Item, Quantity, Price);
               end if;
            end;
         end if;
      end Add_Ask;

   begin
      Harriet.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;

      if Manager.Recipe = Null_Recipe_Reference then
         Manager.Choose_Recipe;
      end if;

      Manager.Scan_Current_Stock (Add_Ask'Access);

   end Create_Market_Offers;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);

      procedure Add_Ask
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type);

      procedure Add_Bid
        (Item     : Harriet.Db.Commodity_Reference);

      -------------
      -- Add_Ask --
      -------------

      procedure Add_Ask
        (Item     : Harriet.Db.Commodity_Reference;
         Quantity : Harriet.Quantities.Quantity_Type;
         Value    : Harriet.Money.Money_Type)
      is
      begin
         Harriet.Logging.Log
           (Actor    => Installation.Identity,
            Location => Harriet.Worlds.Name (Installation.World),
            Category => "market",
            Message  => "creating ask for "
            & Harriet.Commodities.Local_Name (Item)
            & ": quantity "
            & Harriet.Quantities.Show (Quantity)
            & "; value "
            & Harriet.Money.Show (Value));
         Manager.Place_Ask
           (Commodity => Item,
            Quantity  => Quantity,
            Price     =>
              Harriet.Money.Adjust_Price
                (Manager.Current_Agent_Stock_Price (Item), 1.1));
      end Add_Ask;

      -------------
      -- Add_Bid --
      -------------

      procedure Add_Bid
        (Item     : Harriet.Db.Commodity_Reference)
      is
         use Harriet.Quantities;
         Quantity : constant Quantity_Type :=
                      Manager.Current_Market_Ask_Quantity
                        (Item);
      begin
         if Quantity > Manager.Current_Stock (Item) then
            Harriet.Logging.Log
              (Actor    => Installation.Identity,
               Location => Harriet.Worlds.Name (Installation.World),
               Category => "market",
               Message  => "creating bid for "
               & Harriet.Commodities.Local_Name (Item)
               & ": quantity "
               & Harriet.Quantities.Show
                 (Quantity - Manager.Current_Stock (Item))
               & "; price "
               & Harriet.Money.Show
                 (Harriet.Money.Adjust_Price
                      (Manager.Current_Agent_Stock_Price (Item),
                       0.9)));
            Manager.Place_Bid
              (Commodity => Item,
               Quantity  => Quantity - Manager.Current_Stock (Item),
               Price     =>
                 Harriet.Money.Adjust_Price
                   (Manager.Current_Agent_Stock_Price (Item),
                    0.9));
         end if;
      end Add_Bid;

   begin
      Create_Market_Offers (Root_Installation_Manager (Manager));

      Harriet.Logging.Log
        (Actor    => Installation.Identity,
         Location => Harriet.Worlds.Name (Installation.World),
         Category => "market",
         Message  => "scanning stock");

      Harriet.Stock.Scan_Stock
        (Installation, Add_Ask'Access);

      for Item of Harriet.Db.Consumer_Good.Scan_By_Tag loop
         Add_Bid (Item.Get_Commodity_Reference);
      end loop;
      for Item of Harriet.Db.Resource.Scan_By_Tag loop
         Add_Bid (Item.Get_Commodity_Reference);
      end loop;

   end Create_Market_Offers;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager)
   is
      Installation : constant Harriet.Db.Installation.Installation_Type :=
                       Harriet.Db.Installation.Get
                         (Manager.Installation);
      Gen          : constant Harriet.Db.Resource_Generator
        .Resource_Generator_Type :=
          Harriet.Db.Resource_Generator.Get
            (Manager.Rgen);

   begin

      for Deposit of
        Harriet.Db.Deposit.Select_By_World_Sector
          (Installation.World_Sector)
      loop
         declare
            Resource : constant Harriet.Db.Resource_Reference :=
                         Deposit.Resource;
         begin
            if Harriet.Db.Generated_Resource.Is_Generated_Resource
              (Gen.Get_Resource_Generator_Reference, Resource)
            then
               declare
                  use Harriet.Quantities;
                  Commodity : constant Harriet.Db.Commodity_Reference :=
                                Harriet.Db.Resource.Get (Resource)
                                .Get_Commodity_Reference;
                  Quantity : constant Quantity_Type :=
                               To_Quantity
                                 (Deposit.Accessibility * 20.0
                                  * (Harriet.Random.Unit_Random + 0.5));
                  Cost     : constant Harriet.Money.Money_Type :=
                               Harriet.Money.Total
                                 (Manager.Current_Market_Bid_Price
                                    (Commodity),
                                  Quantity);
               begin
                  Harriet.Stock.Add_Stock
                    (Installation, Commodity, Quantity, Cost);
               end;
            end if;
         end;
      end loop;

   end Execute_Agent_Tasks;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Hub_Manager)
   is
      procedure Check_Trend (Commodity : Harriet.Db.Commodity_Reference);

      -----------------
      -- Check_Trend --
      -----------------

      procedure Check_Trend (Commodity : Harriet.Db.Commodity_Reference) is

         Series : Harriet.Data_Series.Series;

         procedure Add_Data_Point
           (Date     : Harriet.Calendar.Time;
            Quantity : Harriet.Quantities.Quantity_Type);

         --------------------
         -- Add_Data_Point --
         --------------------

         procedure Add_Data_Point
           (Date     : Harriet.Calendar.Time;
            Quantity : Harriet.Quantities.Quantity_Type)
         is
         begin
            Harriet.Data_Series.Add_Point
              (Series,
               Harriet.Calendar.To_Real (Date),
               Harriet.Quantities.To_Real (Quantity));
         end Add_Data_Point;

      begin
         Manager.Scan_Historical_Stock
           (Commodity, 7.0, Add_Data_Point'Access);

         Add_Data_Point (Harriet.Calendar.Clock,
                         Manager.Current_Stock (Commodity));

         declare
            Trend : constant Harriet.Data_Series.Regression :=
                      Harriet.Data_Series.Simple_Linear_Regression
                        (Series);
            Gradient : constant Real :=
                         Harriet.Data_Series.Gradient (Trend);
         begin
            if abs Gradient > 0.001 then
               declare
                  use Harriet.Calendar;
                  Zero_Date : constant Harriet.Calendar.Time :=
                                Harriet.Calendar.To_Time
                                  (Harriet.Data_Series.X_Intercept (Trend));
                  Now       : constant Harriet.Calendar.Time :=
                                Harriet.Calendar.Clock;
                  Max_Increase : constant Harriet.Calendar.Time :=
                                   Now + Days (7.0);
                  Min_Increase : constant Harriet.Calendar.Time :=
                                   Now + Days (70.0);
                  Adjustment   : Non_Negative_Real := 1.0;
                  Old_Price    : constant Harriet.Money.Price_Type :=
                                   Manager.Current_Agent_Stock_Price
                                     (Commodity);
               begin
                  if Zero_Date > Now then
                     if Zero_Date < Max_Increase then
                        Adjustment := 1.5;
                     elsif Zero_Date < Min_Increase then
                        Adjustment :=
                          1.0 +
                            Real (Min_Increase - Zero_Date)
                          / Real (Days (1))
                          / 500.0;
                     end if;

                     if Adjustment /= 1.0 then
                        Manager.Set_Agent_Stock_Price
                          (Commodity,
                           Harriet.Money.Adjust_Price
                             (Manager.Current_Agent_Stock_Price (Commodity),
                              Adjustment));

                        Harriet.Logging.Log
                          (Actor    =>
                             Harriet.Db.Facility.Get
                               (Manager.Facility).Tag,
                           Location =>
                             Harriet.Worlds.Name (Manager.World),
                           Category =>
                             Harriet.Commodities.Local_Name (Commodity),
                           Message  =>
                             "stock exhausted within "
                           & Harriet.Real_Images.Approximate_Image
                             (Real (Zero_Date - Now) / Real (Days (1)))
                           & " days on "
                           & Harriet.Calendar.Image (Zero_Date)
                           & "; adjustment +"
                           & Harriet.Real_Images.Approximate_Image
                             ((Adjustment - 1.0) * 100.0)
                           & "%"
                           & "; old price "
                           & Harriet.Money.Show (Old_Price)
                           & "; new price "
                           & Harriet.Money.Show
                             (Manager.Current_Agent_Stock_Price
                                  (Commodity)));
                     end if;
                  elsif Harriet.Data_Series.Gradient (Trend) > 0.5 then
                     declare
                        D : constant Unit_Real :=
                              1.0 - Real'Min (Gradient, 5.0) / 50.0;
                     begin
                        Manager.Set_Agent_Stock_Price
                          (Commodity,
                           Harriet.Money.Adjust_Price
                             (Manager.Current_Agent_Stock_Price (Commodity),
                              D));
                        Harriet.Logging.Log
                          (Actor    =>
                             Harriet.Db.Facility.Get
                               (Manager.Facility).Tag,
                           Location =>
                             Harriet.Worlds.Name (Manager.World),
                           Category =>
                             Harriet.Commodities.Local_Name (Commodity),
                           Message  =>
                             "stock growing at "
                           & Harriet.Real_Images.Approximate_Image (Gradient)
                           & "; adjustment -"
                           & Harriet.Real_Images.Approximate_Image
                             ((1.0 - D) * 100.0)
                           & "%"
                           & "; old price "
                           & Harriet.Money.Show (Old_Price)
                           & "; new price "
                           & Harriet.Money.Show
                             (Manager.Current_Agent_Stock_Price (Commodity)));
                     end;
                  end if;
               end;
            end if;
         end;
      end Check_Trend;

   begin
      Manager.Day_Tick := Manager.Day_Tick + 1;
      if Manager.Day_Tick = 7 then
         for Item of Harriet.Db.Consumer_Good.Scan_By_Tag loop
            Check_Trend (Item.Get_Commodity_Reference);
         end loop;
         for Item of Harriet.Db.Resource.Scan_By_Tag loop
            Check_Trend (Item.Get_Commodity_Reference);
         end loop;

         Manager.Day_Tick := 0;
      end if;

      if Manager.Log_State then
         Manager.Log_Market_State;
      end if;

   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Harriet.Commodities.Stock_Type)
   is
   begin
      for Employee of
        Harriet.Db.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            Commodity : constant Harriet.Db.Commodity_Reference :=
                          Harriet.Db.Pop_Group.Get (Employee.Pop_Group)
                          .Get_Commodity_Reference;
            Quantity  : constant Harriet.Quantities.Quantity_Type :=
                          Harriet.Quantities.Scale
                            (Employee.Quantity, Manager.Capacity);
            Price     : constant Harriet.Money.Price_Type :=
                          Manager.Current_Market_Ask_Price (Commodity);
         begin
            Stock.Set_Quantity
              (Commodity, Quantity, Harriet.Money.Total (Price, Quantity));
         end;
      end loop;
   end Get_Required_Stock;

end Harriet.Managers.Installations;
