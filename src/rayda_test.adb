with Rayda;
with Rayda_Types;
with Interfaces.C; use Interfaces.C;

procedure Rayda_Rectangle_Test is

   Screen_Width  : constant := 800;
   Screen_Height : constant := 450;
   Background    : constant Rayda_Types.Color :=
     (r => unsigned_char(0),
      g => unsigned_char(0),
      b => unsigned_char(255),
      a => unsigned_char(255));

   Rect_Color    : aliased Rayda_Types.Color :=
     (r => unsigned_char(255),
      g => unsigned_char(0),
      b => unsigned_char(0),
      a => unsigned_char(255));

begin
   Rayda.Init_Window (Screen_Width, Screen_Height, "Rayda Test");
   Rayda.Set_Target_FPS (60);

   while Rayda.Window_Should_Close = 0 loop
      Rayda.Begin_Drawing;

      Rayda.Clear_Background (Background);
      Rayda.Draw_Rectangle(300, 200, 200, 100, Rect_Color);

      Rayda.End_Drawing;
   end loop;

   Rayda.Close_Window;
end Rayda_Rectangle_Test;
