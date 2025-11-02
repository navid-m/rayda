with Interfaces.C;

package Raylib_Types is
   pragma Pure;

   type Color is record
      r, g, b, a : Interfaces.C.unsigned_char;
   end record;
   
   type Vector2 is record
      x, y : Interfaces.C.C_float;
   end record;
   
   type Vector3 is record
      x, y, z : Interfaces.C.C_float;
   end record;
   
   type Matrix is array (1 .. 4, 1 .. 4) of Interfaces.C.C_float;
   
   type Rectangle is record
      x, y, width, height : Interfaces.C.C_float;
   end record;
   
   type Camera3D is record
      position : Vector3;
      target : Vector3;
      up : Vector3;
      fovy : Interfaces.C.C_float;
      projection : Interfaces.C.int;
   end record;
   
   type Camera2D is record
      offset : Vector2;
      target : Vector2;
      rotation : Interfaces.C.C_float;
      zoom : Interfaces.C.C_float;
   end record;
   
   type Texture is record
      id : Interfaces.C.unsigned;
      width, height : Interfaces.C.int;
      mipmaps : Interfaces.C.int;
      format : Interfaces.C.int;
   end record;
   
   type Sound is record
      stream : aliased Interfaces.C.int;
      sample_count : Interfaces.C.unsigned;
   end record;
end Raylib_Types;
