with Rayda;
with Rayda_Types;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

procedure Rayda_Test is
   Screen_Width  : constant := 800;
   Screen_Height : constant := 450;

   Background : constant Rayda_Types.Color :=
     (r => unsigned_char (20),
      g => unsigned_char (20),
      b => unsigned_char (40),
      a => unsigned_char (255));

   Snow_Color : constant Rayda_Types.Color :=
     (r => unsigned_char (255),
      g => unsigned_char (255),
      b => unsigned_char (255),
      a => unsigned_char (255));

   Snow_Count : constant := 200;

   type Float_Array is array (1 .. Snow_Count) of Interfaces.C.C_float;

   Snow_X  : Float_Array;
   Snow_Y  : Float_Array;
   Speed_Y : Float_Array;

begin
   Rayda.Init_Window
     (Screen_Width, Screen_Height, Interfaces.C.To_C ("Snowfall" & ASCII.NUL));
   Rayda.Set_Target_FPS (60);

   for I in 1 .. Snow_Count loop
      Snow_X (I) :=
        Interfaces.C.C_float (Rayda.Get_Random_Value (0, Screen_Width));
      Snow_Y (I) :=
        Interfaces.C.C_float (Rayda.Get_Random_Value (0, Screen_Height));
      Speed_Y (I) := Interfaces.C.C_float (Rayda.Get_Random_Value (1, 3));
   end loop;

   while Rayda.Window_Should_Close = 0 loop
      Rayda.Begin_Drawing;
      Rayda.Clear_Background (Background);

      for I in 1 .. Snow_Count loop
         Snow_Y (I) := Snow_Y (I) + Speed_Y (I);

         if Snow_Y (I) > Interfaces.C.C_float (Screen_Height) then
            Snow_Y (I) := 0.0;
            Snow_X (I) :=
              Interfaces.C.C_float (Rayda.Get_Random_Value (0, Screen_Width));
         end if;

         Rayda.Draw_Circle
           (Interfaces.C.int (Snow_X (I)),
            Interfaces.C.int (Snow_Y (I)),
            2.0,
            Snow_Color);
      end loop;
      Rayda.Draw_FPS (Interfaces.C.int (11), Interfaces.C.int (11));

      Rayda.End_Drawing;
   end loop;

   Rayda.Close_Window;
end Rayda_Test;
