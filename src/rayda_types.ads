with Interfaces.C;
with Interfaces.C.Strings;
with Rayda_Primitives;

package Rayda_Types is
   pragma Preelaborate;

   subtype Color is Rayda_Primitives.Color;
   subtype Vector2 is Rayda_Primitives.Vector2;
   subtype Vector3 is Rayda_Primitives.Vector3;
   subtype Vector4 is Rayda_Primitives.Vector4;
   subtype Matrix is Rayda_Primitives.Matrix;
   subtype Rectangle is Rayda_Primitives.Rectangle;
   subtype Image is Rayda_Primitives.Image;
   subtype Texture is Rayda_Primitives.Texture;
   subtype Texture2D is Rayda_Primitives.Texture2D;
   subtype Texture_Cubemap is Rayda_Primitives.Texture_Cubemap;
   subtype Shader is Rayda_Primitives.Shader;
   subtype Transform is Rayda_Primitives.Transform;
   subtype Float_Array_4 is Rayda_Primitives.Float_Array_4;
   subtype Float_Array_2 is Rayda_Primitives.Float_Array_2;
   subtype Matrix_Array_2 is Rayda_Primitives.Matrix_Array_2;

   type Render_Texture is record
      id      : Interfaces.C.unsigned;
      texture : Rayda_Primitives.Texture;
      depth   : Rayda_Primitives.Texture;
   end record;
   pragma Convention (C, Render_Texture);

   subtype Render_Texture2D is Render_Texture;

   type Glyph_Info is record
      value     : Interfaces.C.int;
      offset_x  : Interfaces.C.int;
      offset_y  : Interfaces.C.int;
      advance_x : Interfaces.C.int;
      image     : Rayda_Primitives.Image;
   end record;
   pragma Convention (C, Glyph_Info);

   type Font is record
      base_size     : Interfaces.C.int;
      glyph_count   : Interfaces.C.int;
      glyph_padding : Interfaces.C.int;
      texture       : Rayda_Primitives.Texture;
      recs          : access Rayda_Primitives.Rectangle;
      glyphs        : access Glyph_Info;
   end record;
   pragma Convention (C, Font);

   type Camera3D is record
      position   : Rayda_Primitives.Vector3;
      target     : Rayda_Primitives.Vector3;
      up         : Rayda_Primitives.Vector3;
      fovy       : Interfaces.C.C_float;
      projection : Interfaces.C.int;
   end record;
   pragma Convention (C, Camera3D);

   subtype Camera is Camera3D;

   type Camera2D is record
      offset   : Rayda_Primitives.Vector2;
      target   : Rayda_Primitives.Vector2;
      rotation : Interfaces.C.C_float;
      zoom     : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Camera2D);

   type Mesh is record
      vertex_count   : Interfaces.C.int;
      triangle_count : Interfaces.C.int;
      vertices       : access Interfaces.C.C_float;
      texcoords      : access Interfaces.C.C_float;
      texcoords2     : access Interfaces.C.C_float;
      normals        : access Interfaces.C.C_float;
      tangents       : access Interfaces.C.C_float;
      colors         : access Interfaces.C.unsigned_char;
      indices        : access Interfaces.C.unsigned_short;
      anim_vertices  : access Interfaces.C.C_float;
      anim_normals   : access Interfaces.C.C_float;
      bone_ids       : access Interfaces.C.unsigned_char;
      bone_weights   : access Interfaces.C.C_float;
      vao_id         : Interfaces.C.unsigned;
      vbo_id         : access Interfaces.C.unsigned;
   end record;
   pragma Convention (C, Mesh);

   type Material_Map is record
      texture : Rayda_Primitives.Texture;
      color   : Rayda_Primitives.Color;
      value   : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Material_Map);

   type Material is record
      shader : Rayda_Primitives.Shader;
      maps   : access Material_Map;
      params : Rayda_Primitives.Float_Array_4;
   end record;
   pragma Convention (C, Material);

   type Bone_Info is record
      name   : Interfaces.C.char_array (1 .. 32);
      parent : Interfaces.C.int;
   end record;
   pragma Convention (C, Bone_Info);

   type Model is record
      transform      : Rayda_Primitives.Matrix;
      mesh_count     : Interfaces.C.int;
      material_count : Interfaces.C.int;
      meshes         : access Mesh;
      materials      : access Material;
      mesh_material  : access Interfaces.C.int;
      bone_count     : Interfaces.C.int;
      bones          : access Bone_Info;
      bind_pose      : access Rayda_Primitives.Transform;
   end record;
   pragma Convention (C, Model);

   type Model_Animation is record
      bone_count  : Interfaces.C.int;
      frame_count : Interfaces.C.int;
      bones       : access Bone_Info;
      frame_poses : access Rayda_Primitives.Transform;
   end record;
   pragma Convention (C, Model_Animation);

   type Ray is record
      position  : Rayda_Primitives.Vector3;
      direction : Rayda_Primitives.Vector3;
   end record;
   pragma Convention (C, Ray);

   type Ray_Collision is record
      hit      : Interfaces.C.int;
      distance : Interfaces.C.C_float;
      point    : Rayda_Primitives.Vector3;
      normal   : Rayda_Primitives.Vector3;
   end record;
   pragma Convention (C, Ray_Collision);

   type Bounding_Box is record
      min : Rayda_Primitives.Vector3;
      max : Rayda_Primitives.Vector3;
   end record;
   pragma Convention (C, Bounding_Box);

   type Audio_Stream is record
      buffer      : access Interfaces.C.unsigned;
      processor   : access Interfaces.C.unsigned;
      sample_rate : Interfaces.C.unsigned;
      sample_size : Interfaces.C.unsigned;
      channels    : Interfaces.C.unsigned;
   end record;
   pragma Convention (C, Audio_Stream);

   type Sound is record
      stream      : Audio_Stream;
      frame_count : Interfaces.C.unsigned;
   end record;
   pragma Convention (C, Sound);

   type Music is record
      stream      : Audio_Stream;
      frame_count : Interfaces.C.unsigned;
      looping     : Interfaces.C.int;
      ctx_type    : Interfaces.C.int;
      ctx_data    : access Interfaces.C.unsigned_char;
   end record;
   pragma Convention (C, Music);

   type VR_Device_Info is record
      h_resolution             : Interfaces.C.int;
      v_resolution             : Interfaces.C.int;
      h_screen_size            : Interfaces.C.C_float;
      v_screen_size            : Interfaces.C.C_float;
      v_screen_center          : Interfaces.C.C_float;
      eye_to_screen_distance   : Interfaces.C.C_float;
      lens_separation_distance : Interfaces.C.C_float;
      interpupillary_distance  : Interfaces.C.C_float;
      lens_distortion_values   : Rayda_Primitives.Float_Array_4;
      chroma_ab_correction     : Rayda_Primitives.Float_Array_4;
   end record;
   pragma Convention (C, VR_Device_Info);

   type VR_Stereo_Config is record
      projection          : Rayda_Primitives.Matrix_Array_2;
      view_offset         : Rayda_Primitives.Matrix_Array_2;
      left_lens_center    : Rayda_Primitives.Float_Array_2;
      right_lens_center   : Rayda_Primitives.Float_Array_2;
      left_screen_center  : Rayda_Primitives.Float_Array_2;
      right_screen_center : Rayda_Primitives.Float_Array_2;
      scale               : Rayda_Primitives.Float_Array_2;
      scale_in            : Rayda_Primitives.Float_Array_2;
   end record;
   pragma Convention (C, VR_Stereo_Config);

   type File_Path_List is record
      capacity : Interfaces.C.unsigned;
      count    : Interfaces.C.unsigned;
      paths    : access Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, File_Path_List);

end Rayda_Types;
