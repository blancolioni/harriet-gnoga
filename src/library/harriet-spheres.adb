with Ada.Numerics;

with Harriet.Elementary_Functions;
with Harriet.Random;

package body Harriet.Spheres is

   --------------------------
   -- Random_Sphere_Points --
   --------------------------

   procedure Random_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural)
   is
      use Harriet.Elementary_Functions;

      Pi : constant := Ada.Numerics.Pi;

      Sample_Count : Positive := 1;

      Lambdas : array (1 .. Count) of Real;
      Phis    : array (1 .. Count) of Real;

      Current_Count : Natural := 0;

      procedure Next_Point;

      ---------------
      -- New_Point --
      ---------------

      procedure Next_Point is
         Best_Lambda : Real;
         Best_Phi    : Real;
         Best_Min_D  : Real := 0.0;
      begin
         for I in 1 .. Sample_Count loop
            declare
               Lambda : constant Real :=
                          (Harriet.Random.Unit_Random * 2.0 - 1.0) * Pi;
               Phi    : constant Real :=
                          Arccos (2.0 * Harriet.Random.Unit_Random - 1.0);
               Min_Distance : Non_Negative_Real := Real'Last;
            begin
               if I = 1 then
                  Best_Lambda := Lambda;
                  Best_Phi    := Phi;
               else
                  for J in 1 .. Current_Count loop
                     declare
                        Sin_Phi_1 : constant Signed_Unit_Real :=
                                      Sin (Phi);
                        Sin_Phi_2 : constant Signed_Unit_Real :=
                                      Sin (Phis (J));
                        Cos_Phi_1 : constant Signed_Unit_Real :=
                                      Cos (Phi);
                        Cos_Phi_2 : constant Signed_Unit_Real :=
                                      Cos (Phis (J));
                        D_Lambda  : constant Non_Negative_Real :=
                                      abs (Lambda - Lambdas (J));
                        Cos_D_Lambda : constant Signed_Unit_Real :=
                                        Cos (D_Lambda);
                        Cos_D     : constant Signed_Unit_Real :=
                                      Sin_Phi_1 * Sin_Phi_2
                                          + Cos_Phi_1 * Cos_Phi_2
                                        * Cos_D_Lambda;
                        D         : constant Non_Negative_Real :=
                                      Arccos (Cos_D);
                     begin
                        if D < Min_Distance then
                           Min_Distance := D;
                        end if;
                     end;
                  end loop;
               end if;

               if Min_Distance > Best_Min_D then
                  Best_Min_D := Min_Distance;
                  Best_Lambda := Lambda;
                  Best_Phi := Phi;
               end if;
            end;
         end loop;

         Current_Count := Current_Count + 1;
         Lambdas (Current_Count) := Best_Lambda;
         Phis (Current_Count) := Best_Phi;
         Sample_Count := Sample_Count + 1;
      end Next_Point;

   begin
      for I in 1 .. Count loop
         Next_Point;
      end loop;

      for I in 1 .. Count loop
         Point_List.Append
           (Surface_Point'
              (X => Cos (Lambdas (I)) * Cos (Phis (I)),
               Y => Sin (Lambdas (I)) * Cos (Phis (I)),
               Z => Sin (Phis (I))));
      end loop;
   end Random_Sphere_Points;

end Harriet.Spheres;
