-- Flight Type Constraints
-- Practice distinct numeric types and constrained subtypes.

with Ada.Text_IO; use Ada.Text_IO;

procedure Flight_Type_Constraints is
   type Feet is new Integer;
   type Meters is new Integer;
   type Knots is new Float;

   subtype Cruise_Altitude is Feet range 10_000 .. 45_000;
   subtype Operational_Airspeed is Knots range 0.0 .. 450.0;

   function To_Feet (Value : Meters) return Feet is
   begin
      return Feet (Float (Value) * 3.28084);
   end To_Feet;

   function To_Meters (Value : Feet) return Meters is
   begin
      return Meters (Float (Value) / 3.28084);
   end To_Meters;

   Measured_Altitude : Meters := 10_000;
   Cruise_Level      : Cruise_Altitude := To_Feet (Measured_Altitude);
   Converted_Altitude : Meters := To_Meters (Cruise_Level);
   Airspeed          : Operational_Airspeed := 250.0;
begin
   Put_Line ("Altitude in feet:" &
     Integer'Image (Integer (Cruise_Level)));
   Put_Line ("Converted altitude in meters:" &
     Integer'Image (Integer (Converted_Altitude)));
   Put_Line ("Airspeed in knots:" &
     Float'Image (Float (Airspeed)));

   begin
      Cruise_Level := Feet (50_000);
   exception
      when Constraint_Error =>
         Put_Line ("Rejected altitude outside the cruise range.");
   end;

   begin
      Airspeed := Knots (500.0);
   exception
      when Constraint_Error =>
         Put_Line ("Rejected airspeed outside the operational range.");
   end;
end Flight_Type_Constraints;
