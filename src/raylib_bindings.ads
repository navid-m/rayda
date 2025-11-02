with Interfaces.C;

package Raylib_Bindings is
   pragma Pure;

   type Color is record
      r, g, b, a : Interfaces.C.unsigned_char;
   end record;
   
   procedure Init_Window(width, height : Interfaces.C.int; title : Interfaces.C.char_array);
   pragma Import (C, Init_Window, "InitWindow");
   
   procedure Close_Window;
   pragma Import (C, Close_Window, "CloseWindow");
   
   function Window_Should_Close return Interfaces.C.int;
   pragma Import (C, Window_Should_Close, "WindowShouldClose");
   
   procedure Begin_Drawing;
   pragma Import (C, Begin_Drawing, "BeginDrawing");
   
   procedure End_Drawing;
   pragma Import (C, End_Drawing, "EndDrawing");
end Raylib_Bindings;
