with Interfaces.C;

package Raylib_Bindings is
   pragma Pure;

   type Color is record
      r, g, b, a : Interfaces.C.unsigned_char;
   end record;
   
   procedure InitWindow(width, height : Interfaces.C.int; title : Interfaces.C.char_array);
   pragma Import (C, InitWindow, "InitWindow");
   
   procedure CloseWindow;
   pragma Import (C, CloseWindow, "CloseWindow");
   
   function WindowShouldClose return Interfaces.C.int;
   pragma Import (C, WindowShouldClose, "WindowShouldClose");
   
   procedure BeginDrawing;
   pragma Import (C, BeginDrawing, "BeginDrawing");
   
   procedure EndDrawing;
   pragma Import (C, EndDrawing, "EndDrawing");
end Raylib_Bindings;
