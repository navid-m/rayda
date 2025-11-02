with Interfaces.C;
with Raylib_Types;

package Raylib_Bindings is
   pragma Pure;

   use type Raylib_Types.Color;
   use type Raylib_Types.Vector2;

   procedure Init_Window(width, height : Interfaces.C.int; title : Interfaces.C.char_array);
   pragma Import (C, Init_Window, "InitWindow");

   procedure Close_Window;
   pragma Import (C, Close_Window, "CloseWindow");

   function Window_Should_Close return Interfaces.C.int;
   pragma Import (C, Window_Should_Close, "WindowShouldClose");

   procedure Set_Target_FPS(fps : Interfaces.C.int);
   pragma Import (C, Set_Target_FPS, "SetTargetFPS");

   procedure Begin_Drawing;
   pragma Import (C, Begin_Drawing, "BeginDrawing");

   procedure End_Drawing;
   pragma Import (C, End_Drawing, "EndDrawing");

   procedure Clear_Background(color : Raylib_Types.Color);
   pragma Import (C, Clear_Background, "ClearBackground");

   procedure Draw_Circle(center_x, center_y : Interfaces.C.int; radius : Interfaces.C.C_float; color : Raylib_Types.Color);
   pragma Import (C, Draw_Circle, "DrawCircle");

   procedure Draw_Rectangle(pos_x, pos_y, width, height : Interfaces.C.int; color : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle, "DrawRectangle");

   function Is_Key_Pressed(key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Pressed, "IsKeyPressed");

   function Get_Mouse_Position return Raylib_Types.Vector2;
   pragma Import (C, Get_Mouse_Position, "GetMousePosition");
end Raylib_Bindings;
