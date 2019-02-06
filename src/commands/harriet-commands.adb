with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Harriet.Commands is

   type Null_Writer_Record is
     new Writer_Interface with null record;

   overriding procedure Put
     (Writer : Null_Writer_Record;
      Text   : String)
   is null;

   overriding procedure New_Line
     (Writer : Null_Writer_Record)
   is null;

   overriding procedure Put_Error
     (Writer : Null_Writer_Record;
      Text   : String);

   package Command_Maps is
     new WL.String_Maps (Root_Harriet_Command'Class);

   Map : Command_Maps.Map;

   procedure Set_Flag
     (Arguments : in out Argument_List;
      Flag      : String);

   procedure Set_Named_Value
     (Arguments : in out Argument_List;
      Name      : String;
      Value     : String);

   procedure Set_Value
     (Arguments : in out Argument_List;
      Value     : String);

   function Scan_Arguments
     (Argument_Line : String)
      return Argument_List;

   --------------------------
   -- Execute_Command_Line --
   --------------------------

   procedure Execute_Command_Line
     (Line    : String;
      Session : Harriet.Sessions.Harriet_Session;
      Writer  : Writer_Interface'Class)
   is
   begin
      if Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both) = "" then
         return;
      end if;

      declare
         Extended_Line : constant String := Line & ' ';
         First         : constant Positive :=
                           Ada.Strings.Fixed.Index_Non_Blank (Extended_Line);
         Index         : constant Positive :=
                           Ada.Strings.Fixed.Index (Extended_Line, " ", First);
         Command_Name  : constant String :=
                          Extended_Line
                            (Extended_Line'First .. Index - 1);
      begin
         if not Map.Contains (Command_Name) then
            Writer.Put_Error (Command_Name & ": command not found");
            return;
         end if;

         declare
            Command   : constant Root_Harriet_Command'Class :=
                          Map.Element (Command_Name);
            Arguments : constant Argument_List :=
                          Scan_Arguments
                            (Extended_Line (Index + 1 .. Extended_Line'Last));
         begin
            Command.Execute (Session, Writer, Arguments);
         end;
      end;

   end Execute_Command_Line;

   -----------------
   -- Null_Writer --
   -----------------

   function Null_Writer return Writer_Interface'Class is
   begin
      return Writer : Null_Writer_Record;
   end Null_Writer;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer : Null_Writer_Record;
      Text   : String)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "ERROR: " & Text);
   end Put_Error;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : Writer_Interface'Class;
      Text   : String)
   is
   begin
      Writer.Put (Text);
      Writer.New_Line;
   end Put_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Command_Name : String;
      Command      : Root_Harriet_Command'Class)
   is
   begin
      Map.Insert (Command_Name, Command);
   end Register;

   --------------------
   -- Scan_Arguments --
   --------------------

   function Scan_Arguments
     (Argument_Line : String)
      return Argument_List
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Extended_Line : constant String :=
                        Trim (Argument_Line, Both) & ' ';
      First         : Natural := Extended_Line'First;
      Last          : Natural := Index (Extended_Line, " ") - 1;
   begin
      return Arguments : Argument_List do
         while First > 0 and then Last >= First loop
            declare
               Arg : constant String := Extended_Line (First .. Last);
            begin
               First := Index_Non_Blank (Extended_Line, Last + 1);
               Last := (if First = 0 then 0
                        else Index (Extended_Line, " ", First) - 1);

               if Arg = "-" or else Arg = "--" then
                  null;
               elsif Arg (Arg'First) = '-'
                 and then Arg (Arg'First + 1) = '-'
               then
                  declare
                     Equal_Index : constant Natural :=
                                     Index (Arg, "=");
                  begin
                     if Equal_Index = 0 then
                        Set_Flag (Arguments, Arg (Arg'First + 2 .. Arg'Last));
                     else
                        declare
                           Name : constant String :=
                                    Arg (Arg'First + 2 .. Equal_Index - 1);
                           Value : constant String :=
                                     Arg (Equal_Index + 1 .. Arg'Last);
                        begin
                           Set_Named_Value
                             (Arguments => Arguments,
                              Name      => Name,
                              Value     => Value);
                        end;
                     end if;
                  end;
               elsif Arg (Arg'First) = '-' then
                  for Ch of Arg (Arg'First + 1 .. Arg'Last) loop
                     Set_Flag (Arguments, (1 => Ch));
                  end loop;
               else
                  Set_Value (Arguments, Arg);
               end if;
            end;
         end loop;
      end return;
   end Scan_Arguments;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Arguments : in out Argument_List;
      Flag      : String)
   is
   begin
      Arguments.Map.Insert (Flag, "");
   end Set_Flag;

   ---------------------
   -- Set_Named_Value --
   ---------------------

   procedure Set_Named_Value
     (Arguments : in out Argument_List;
      Name      : String;
      Value     : String)
   is
   begin
      Arguments.Map.Insert (Name, Value);
   end Set_Named_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Arguments : in out Argument_List;
      Value     : String)
   is
   begin
      Arguments.Vector.Append (Value);
   end Set_Value;

end Harriet.Commands;
