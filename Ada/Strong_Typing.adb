-- Strong Typing
-- Date : 2026.11.25
-- Demonstrates distinct numeric types, range constraints, and explicit
-- unit conversions in a flight-monitoring example.

with Ada.Text_IO; use Ada.Text_IO;

procedure Strong_Typing is
   -- Distinct types prevent accidental mixing of different units.
   type Feet is new Integer;
   type Meters is new Integer;
   type Knots is new Float;

   -- Subtypes constrain values to safe operating ranges.
   subtype Cruise_Altitude is Feet range 10_000 .. 45_000;
   subtype Operational_Airspeed is Knots range 0.0 .. 450.0;

   -- Convert through Float to support fractional unit scaling.
   function To_Feet (Value : Meters) return Feet is
   begin
      return Feet (Float (Value) * 3.28084);
   end To_Feet;

   -- Convert back to whole meters.
   function To_Meters (Value : Feet) return Meters is
   begin
      return Meters (Float (Value) / 3.28084);
   end To_Meters;

   Measured_Altitude : Meters := 10_000;
   Cruise_Level      : Cruise_Altitude := To_Feet (Measured_Altitude);
   Converted_Altitude : Meters := To_Meters (Cruise_Level);
   Airspeed          : Operational_Airspeed := 250.0;
begin
   -- Uncomment either line to see a compile-time type mismatch error:
   -- Cruise_Level := Measured_Altitude;
   -- Cruise_Level := Cruise_Level + Airspeed;

   Put_Line ("Altitude in feet:" &
     Integer'Image (Integer (Cruise_Level)));
   Put_Line ("Converted altitude in meters:" &
     Integer'Image (Integer (Converted_Altitude)));
   Put_Line ("Airspeed in knots:" &
     Float'Image (Float (Airspeed)));

   -- Out-of-range assignments raise Constraint_Error; catch it for the demo.
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
end Strong_Typing;
