with Interfaces.C;

package Raylib_Types is
   pragma Preelaborate;

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
   
   type Texture is private;
   type Audio_Stream is private;
   type Sound is private;
   type Camera3D is private;
   type Camera2D is private;
   type Font is private;

private
   type Texture is record
      id : Interfaces.C.unsigned;
      width : Interfaces.C.int;
      height : Interfaces.C.int;
      mipmaps : Interfaces.C.int;
      format : Interfaces.C.int;
   end record;
   
   type Texture_Access is access all Texture;
   
   type Font is record
      base_size : Interfaces.C.int;
      glyph_count : Interfaces.C.int;
      glyph_padding : Interfaces.C.int;
      texture : Texture_Access;
      recs : access Rectangle;
      glyphs : access Integer;
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
   
   type Audio_Stream is record
      buffer : access Interfaces.C.short;
      processor : access Interfaces.C.unsigned;
      sample_rate : Interfaces.C.unsigned;
      sample_size : Interfaces.C.unsigned;
      channels : Interfaces.C.unsigned;
   end record;
   
   type Sound is record
      stream : Audio_Stream;
      frame_count : Interfaces.C.unsigned;
   end record;
end Raylib_Types;