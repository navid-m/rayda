with Interfaces.C;

package Raylib_Types is
   pragma Pure;

   type Color is record
      r, g, b, a : Interfaces.C.unsigned_char;
   end record;
   
   type Vector2 is record
      x, y : Interfaces.C.C_float;
   end record;
end Raylib_Types;
