with Raylib_Bindings; use Raylib_Bindings;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   Screen_Width  : constant := 800;
   Screen_Height : constant := 450;
begin
   InitWindow(Screen_Width, Screen_Height, To_C("Raylib Ada Binding Test"));
   
   while WindowShouldClose = 0 loop
      BeginDrawing;
      EndDrawing;
   end loop;
   
   CloseWindow;
end Main;
