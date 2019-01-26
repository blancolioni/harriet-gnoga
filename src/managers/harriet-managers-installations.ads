package Harriet.Managers.Installations is

   function Create
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Installation_Manager is
     abstract new Root_Manager_Type with
      record
         Installation : Harriet.Db.Installation_Reference;
      end record;

end Harriet.Managers.Installations;
