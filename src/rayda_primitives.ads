with Interfaces.C;

package Rayda_Primitives is
   pragma Pure;

   type Color is record
      r, g, b, a : Interfaces.C.unsigned_char;
   end record;
   pragma Convention (C, Color);

   type Vector2 is record
      x, y : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Vector2);

   type Vector3 is record
      x, y, z : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Vector3);

   type Vector4 is record
      x, y, z, w : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Vector4);

   type Matrix is array (1 .. 4, 1 .. 4) of Interfaces.C.C_float;
   pragma Convention (C, Matrix);

   type Rectangle is record
      x, y, width, height : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Rectangle);

   type Image is record
      data    : access Interfaces.C.unsigned_char;
      width   : Interfaces.C.int;
      height  : Interfaces.C.int;
      mipmaps : Interfaces.C.int;
      format  : Interfaces.C.int;
   end record;
   pragma Convention (C, Image);

   type Texture is record
      id      : Interfaces.C.unsigned;
      width   : Interfaces.C.int;
      height  : Interfaces.C.int;
      mipmaps : Interfaces.C.int;
      format  : Interfaces.C.int;
   end record;
   pragma Convention (C, Texture);

   subtype Texture2D is Texture;
   subtype Texture_Cubemap is Texture;

   type Shader is record
      id   : Interfaces.C.unsigned;
      locs : access Interfaces.C.int;
   end record;
   pragma Convention (C, Shader);

   type Transform is record
      translation : Vector3;
      rotation    : Vector4;
      scale       : Vector3;
   end record;
   pragma Convention (C, Transform);

   type Float_Array_4 is array (1 .. 4) of Interfaces.C.C_float;
   pragma Convention (C, Float_Array_4);

   type Float_Array_2 is array (1 .. 2) of Interfaces.C.C_float;
   pragma Convention (C, Float_Array_2);

   type Matrix_Array_2 is array (1 .. 2) of Matrix;
   pragma Convention (C, Matrix_Array_2);

end Rayda_Primitives;