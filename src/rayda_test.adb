with Raylib_Bindings; use Raylib_Bindings;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   Screen_Width  : constant := 800;
   Screen_Height : constant := 450;
begin
   Init_Window(Screen_Width, Screen_Height, To_C("Raylib Ada Binding Test"));
   
   while Window_Should_Close = 0 loop
      Begin_Drawing;
      End_Drawing;
   end loop;
   
   Close_Window;
end Main;
