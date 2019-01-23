private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Harriet.UI.Models.Tables is

   type Abstract_Cell_Interface is interface;

   function To_String
     (Cell : Abstract_Cell_Interface)
      return String
      is abstract;

   type Table_Row_Count is new Natural;
   subtype Table_Row_Index is
     Table_Row_Count range 1 .. Table_Row_Count'Last;

   type Table_Column_Count is new Natural;
   subtype Table_Column_Index is
     Table_Column_Count range 1 .. Table_Column_Count'Last;

   type Root_Table_Model is
     new Root_Harriet_Model with private;

   type Table_Model is access all Root_Table_Model'Class;

   overriding function Title
     (Model : Root_Table_Model)
      return String;

   function Row_Count
     (Table : Root_Table_Model'Class)
      return Table_Row_Count;

   function Column_Count
     (Table : Root_Table_Model'Class)
      return Table_Column_Count;

   function Column_Name
     (Table  : Root_Table_Model'Class;
      Column : Table_Column_Index)
      return String;

   function Image
     (Table  : Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index)
      return String;

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Boolean);

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Integer);

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Real);

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : String);

   procedure Add_Column
     (Table : in out Root_Table_Model'Class;
      Name  : String);

   function Add_Row
     (Table : in out Root_Table_Model'Class)
      return Table_Row_Index;

private

   type Cell_Type is
     (String_Cell,
      Integer_Cell,
      Real_Cell,
      Boolean_Cell,
      Abstract_Cell);

   type Cell_Record (T : Cell_Type) is
      record
         case T is
            when String_Cell =>
               String_Value   : Ada.Strings.Unbounded.Unbounded_String;
            when Integer_Cell =>
               Integer_Value  : Integer;
            when Real_Cell =>
               Real_Value     : Real;
            when Boolean_Cell =>
               Boolean_Value  : Boolean;
            when Abstract_Cell =>
               Abstract_Value : access Abstract_Cell_Interface'Class;
         end case;
      end record;

   function Image (Cell : Cell_Record) return String;

   package Cell_Vectors is
     new Ada.Containers.Indefinite_Vectors (Table_Column_Index, Cell_Record);

   type Row_Record is
      record
         Cells        : Cell_Vectors.Vector;
      end record;

   package Column_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Table_Column_Index, String);

   package Row_Vectors is
     new Ada.Containers.Vectors (Table_Row_Index, Row_Record);

   type Root_Table_Model is
     new Root_Harriet_Model with
      record
         Column_Names : Column_Name_Vectors.Vector;
         Rows         : Row_Vectors.Vector;
      end record;

   function Row_Count
     (Table : Root_Table_Model'Class)
      return Table_Row_Count
   is (Table.Rows.Last_Index);

   function Column_Count
     (Table : Root_Table_Model'Class)
      return Table_Column_Count
   is (Table.Column_Names.Last_Index);

   function Image
     (Table  : Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index)
      return String
   is (Image (Table.Rows (Row).Cells (Column)));

end Harriet.UI.Models.Tables;
