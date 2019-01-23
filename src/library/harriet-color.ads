package Harriet.Color is

   type Harriet_Color is
      record
         Red, Green, Blue, Alpha : Unit_Real;
      end record;

   Black : constant Harriet_Color := (0.0, 0.0, 0.0, 1.0);
   White : constant Harriet_Color := (1.0, 1.0, 1.0, 1.0);

   function From_String
     (Item : String)
      return Harriet_Color;

   function To_Html_String
     (Color : Harriet_Color)
      return String;

end Harriet.Color;
