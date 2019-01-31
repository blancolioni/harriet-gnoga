with Harriet.Calendar;
with Harriet.Logging;
with Harriet.Money;

with Harriet.Agents;
with Harriet.Worlds;

with Harriet.Db.Account;

package body Harriet.Pops is

   ---------------
   -- Move_Pops --
   ---------------

   procedure Move_Pops
     (From     : Harriet.Db.Pop.Pop_Type;
      To       : Harriet.Db.Pop.Pop_Type;
      Quantity : Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Quantities;
   begin
      Harriet.Logging.Log
        (Actor    => From.Identity,
         Location => Harriet.Worlds.Name (From.World),
         Category => "move",
         Message  =>
           Harriet.Quantities.Show (Quantity)
         & "/" & Harriet.Quantities.Show (From.Size)
         & " to "
         & To.Identity
         & " size "
         & Harriet.Quantities.Show (To.Size));

      Harriet.Agents.Move_Assets
        (From, To, To_Real (Quantity) / To_Real (From.Size));
      To.Set_Happiness
        ((To.Happiness * To_Real (To.Size)
         + From.Happiness * To_Real (Quantity))
         / To_Real (To.Size + Quantity));

      From.Set_Capacity (From.Capacity - Quantity);
      To.Set_Capacity (To.Capacity + Quantity);
      From.Set_Size (From.Size - Quantity);
      To.Set_Size (To.Size + Quantity);
   end Move_Pops;

   -------------------
   -- New_Empty_Pop --
   -------------------

   function New_Empty_Pop
     (Faction : Harriet.Db.Faction_Reference;
      Group   : Harriet.Db.Pop_Group_Reference;
      World   : Harriet.Db.World_Reference;
      Sector  : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Pop.Pop_Type
   is
      Account   : constant Harriet.Db.Account_Reference :=
                    Harriet.Db.Account.Create
                      (Harriet.Db.Null_Account_Reference,
                       Harriet.Money.Zero, Harriet.Money.Zero);
      Reference : constant Harriet.Db.Pop_Reference :=
                    Harriet.Db.Pop.Create
                      (Transported_Size => 0.0,
                       Active           => True,
                       Scheduled        => False,
                       Next_Event       => Harriet.Calendar.Clock,
                       Manager          => "default-pop",
                       Account          => Account,
                       Capacity         => Harriet.Quantities.Zero,
                       Faction          => Faction,
                       World            => World,
                       World_Sector     => Sector,
                       Installation     =>
                         Harriet.Db.Null_Installation_Reference,
                       Ship             =>
                         Harriet.Db.Null_Ship_Reference,
                       Pop_Group        => Group,
                       Size             => Harriet.Quantities.Zero,
                       Salary           => Harriet.Money.Zero,
                       Happiness        => 1.0);
   begin
      return Pop : constant Harriet.Db.Pop.Pop_Type :=
        Harriet.Db.Pop.Get (Reference);
   end New_Empty_Pop;

end Harriet.Pops;
