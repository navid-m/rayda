with Interfaces.C;
with Rayda_Types;

package Rayda is
   pragma Preelaborate;

   use type Rayda_Types.Color;
   use type Rayda_Types.Vector2;
   use type Rayda_Types.Vector3;
   use type Rayda_Types.Rectangle;
   use type Rayda_Types.Texture;
   use type Rayda_Types.Sound;
   use type Rayda_Types.Font;
   use type Rayda_Types.Camera2D;
   use type Rayda_Types.Camera3D;

   -- Window-related functions
   procedure Init_Window
     (width, height : Interfaces.C.int; title : Interfaces.C.char_array);
   pragma Import (C, Init_Window, "InitWindow");

   procedure Close_Window;
   pragma Import (C, Close_Window, "CloseWindow");

   function Window_Should_Close return Interfaces.C.int;
   pragma Import (C, Window_Should_Close, "WindowShouldClose");

   function Is_Window_Ready return Interfaces.C.int;
   pragma Import (C, Is_Window_Ready, "IsWindowReady");

   function Is_Window_Fullscreen return Interfaces.C.int;
   pragma Import (C, Is_Window_Fullscreen, "IsWindowFullscreen");

   function Is_Window_Hidden return Interfaces.C.int;
   pragma Import (C, Is_Window_Hidden, "IsWindowHidden");

   function Is_Window_Minimized return Interfaces.C.int;
   pragma Import (C, Is_Window_Minimized, "IsWindowMinimized");

   function Is_Window_Maximized return Interfaces.C.int;
   pragma Import (C, Is_Window_Maximized, "IsWindowMaximized");

   function Is_Window_Focused return Interfaces.C.int;
   pragma Import (C, Is_Window_Focused, "IsWindowFocused");

   function Is_Window_Resized return Interfaces.C.int;
   pragma Import (C, Is_Window_Resized, "IsWindowResized");

   procedure Set_Window_Title (title : Interfaces.C.char_array);
   pragma Import (C, Set_Window_Title, "SetWindowTitle");

   procedure Set_Window_Position (x, y : Interfaces.C.int);
   pragma Import (C, Set_Window_Position, "SetWindowPosition");

   procedure Set_Window_Monitor (monitor : Interfaces.C.int);
   pragma Import (C, Set_Window_Monitor, "SetWindowMonitor");

   procedure Set_Window_Min_Size (width, height : Interfaces.C.int);
   pragma Import (C, Set_Window_Min_Size, "SetWindowMinSize");

   procedure Set_Window_Size (width, height : Interfaces.C.int);
   pragma Import (C, Set_Window_Size, "SetWindowSize");

   procedure Toggle_Fullscreen;
   pragma Import (C, Toggle_Fullscreen, "ToggleFullscreen");

   procedure Maximize_Window;
   pragma Import (C, Maximize_Window, "MaximizeWindow");

   procedure Minimize_Window;
   pragma Import (C, Minimize_Window, "MinimizeWindow");

   procedure Restore_Window;
   pragma Import (C, Restore_Window, "RestoreWindow");

   -- Timing-related functions
   procedure Set_Target_FPS (fps : Interfaces.C.int);
   pragma Import (C, Set_Target_FPS, "SetTargetFPS");

   function Get_FPS return Interfaces.C.int;
   pragma Import (C, Get_FPS, "GetFPS");

   function Get_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Time, "GetTime");

   function Get_Frame_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Frame_Time, "GetFrameTime");

   -- Drawing-related functions
   procedure Begin_Drawing;
   pragma Import (C, Begin_Drawing, "BeginDrawing");

   procedure End_Drawing;
   pragma Import (C, End_Drawing, "EndDrawing");

   procedure Clear_Background (color : Rayda_Types.Color);
   pragma Import (C, Clear_Background, "ClearBackground");

   -- Basic shapes drawing
   procedure Draw_Pixel (pos_x, pos_y : Interfaces.C.int; color : Rayda_Types.Color);
   pragma Import (C, Draw_Pixel, "DrawPixel");

   procedure Draw_Pixel_V (position : Rayda_Types.Vector2; color : Rayda_Types.Color);
   pragma Import (C, Draw_Pixel_V, "DrawPixelV");

   procedure Draw_Line
     (start_pos_x, start_pos_y, end_pos_x, end_pos_y : Interfaces.C.int;
      color                                          : Rayda_Types.Color);
   pragma Import (C, Draw_Line, "DrawLine");

   procedure Draw_Line_V
     (start_pos, end_pos : Rayda_Types.Vector2; color : Rayda_Types.Color);
   pragma Import (C, Draw_Line_V, "DrawLineV");

   procedure Draw_Line_Ex
     (start_pos, end_pos : Rayda_Types.Vector2;
      thick              : Interfaces.C.C_float;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Line_Ex, "DrawLineEx");

   -- Circle drawing
   procedure Draw_Circle
     (center_x, center_y : Interfaces.C.int;
      radius             : Interfaces.C.C_float;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Circle, "DrawCircle");

   procedure Draw_Circle_V
     (center : Rayda_Types.Vector2;
      radius : Interfaces.C.C_float;
      color  : Rayda_Types.Color);
   pragma Import (C, Draw_Circle_V, "DrawCircleV");

   procedure Draw_Circle_Lines
     (center_x, center_y : Interfaces.C.int;
      radius             : Interfaces.C.C_float;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Circle_Lines, "DrawCircleLines");

   procedure Draw_Circle_Gradient
     (center_x, center_y : Interfaces.C.int;
      radius             : Interfaces.C.C_float;
      color1, color2     : Rayda_Types.Color);
   pragma Import (C, Draw_Circle_Gradient, "DrawCircleGradient");

   procedure Draw_Circle_Sector
     (center      : Rayda_Types.Vector2;
      radius      : Interfaces.C.C_float;
      start_angle : Interfaces.C.C_float;
      end_angle   : Interfaces.C.C_float;
      segments    : Interfaces.C.int;
      color       : Rayda_Types.Color);
   pragma Import (C, Draw_Circle_Sector, "DrawCircleSector");

   procedure Draw_Circle_Sector_Lines
     (center      : Rayda_Types.Vector2;
      radius      : Interfaces.C.C_float;
      start_angle : Interfaces.C.C_float;
      end_angle   : Interfaces.C.C_float;
      segments    : Interfaces.C.int;
      color       : Rayda_Types.Color);
   pragma Import (C, Draw_Circle_Sector_Lines, "DrawCircleSectorLines");

   procedure Draw_Ring
     (center            : Rayda_Types.Vector2;
      inner_radius      : Interfaces.C.C_float;
      outer_radius      : Interfaces.C.C_float;
      start_angle       : Interfaces.C.C_float;
      end_angle         : Interfaces.C.C_float;
      segments          : Interfaces.C.int;
      color             : Rayda_Types.Color);
   pragma Import (C, Draw_Ring, "DrawRing");

   procedure Draw_Ring_Lines
     (center            : Rayda_Types.Vector2;
      inner_radius      : Interfaces.C.C_float;
      outer_radius      : Interfaces.C.C_float;
      start_angle       : Interfaces.C.C_float;
      end_angle         : Interfaces.C.C_float;
      segments          : Interfaces.C.int;
      color             : Rayda_Types.Color);
   pragma Import (C, Draw_Ring_Lines, "DrawRingLines");

   -- Rectangle drawing
   procedure Draw_Rectangle
     (pos_x, pos_y, width, height : Interfaces.C.int;
      color                       : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle, "DrawRectangle");

   procedure Draw_Rectangle_V
     (position : Rayda_Types.Vector2;
      size     : Rayda_Types.Vector2;
      color    : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_V, "DrawRectangleV");

   procedure Draw_Rectangle_Rec
     (rec : Rayda_Types.Rectangle; color : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Rec, "DrawRectangleRec");

   procedure Draw_Rectangle_Pro
     (rec      : Rayda_Types.Rectangle;
      origin   : Rayda_Types.Vector2;
      rotation : Interfaces.C.C_float;
      color    : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Pro, "DrawRectanglePro");

   procedure Draw_Rectangle_Lines
     (pos_x, pos_y, width, height : Interfaces.C.int;
      color                       : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Lines, "DrawRectangleLines");

   procedure Draw_Rectangle_Lines_Ex
     (rec       : Rayda_Types.Rectangle;
      line_thick : Interfaces.C.C_float;
      color     : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Lines_Ex, "DrawRectangleLinesEx");

   procedure Draw_Rectangle_Rounded
     (rec       : Rayda_Types.Rectangle;
      roundness : Interfaces.C.C_float;
      segments  : Interfaces.C.int;
      color     : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Rounded, "DrawRectangleRounded");

   procedure Draw_Rectangle_Rounded_Lines
     (rec        : Rayda_Types.Rectangle;
      roundness  : Interfaces.C.C_float;
      segments   : Interfaces.C.int;
      line_thick : Interfaces.C.C_float;
      color      : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Rounded_Lines, "DrawRectangleRoundedLines");

   procedure Draw_Rectangle_Gradient
     (pos_x, pos_y, width, height : Interfaces.C.int;
      color1, color2              : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient, "DrawRectangleGradient");

   procedure Draw_Rectangle_Gradient_V
     (pos_x, pos_y, width, height : Interfaces.C.int;
      color1, color2              : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient_V, "DrawRectangleGradientV");

   procedure Draw_Rectangle_Gradient_H
     (pos_x, pos_y, width, height : Interfaces.C.int;
      color1, color2              : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient_H, "DrawRectangleGradientH");

   procedure Draw_Rectangle_Gradient_Ex
     (rec                   : Rayda_Types.Rectangle;
      col1, col2, col3, col4 : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient_Ex, "DrawRectangleGradientEx");

   -- Triangle and polygon drawing
   procedure Draw_Triangle
     (v1, v2, v3 : Rayda_Types.Vector2; color : Rayda_Types.Color);
   pragma Import (C, Draw_Triangle, "DrawTriangle");

   procedure Draw_Triangle_Lines
     (v1, v2, v3 : Rayda_Types.Vector2; color : Rayda_Types.Color);
   pragma Import (C, Draw_Triangle_Lines, "DrawTriangleLines");

   procedure Draw_Poly
     (center   : Rayda_Types.Vector2;
      sides    : Interfaces.C.int;
      radius   : Interfaces.C.C_float;
      rotation : Interfaces.C.C_float;
      color    : Rayda_Types.Color);
   pragma Import (C, Draw_Poly, "DrawPoly");

   procedure Draw_Poly_Lines
     (center   : Rayda_Types.Vector2;
      sides    : Interfaces.C.int;
      radius   : Interfaces.C.C_float;
      rotation : Interfaces.C.C_float;
      color    : Rayda_Types.Color);
   pragma Import (C, Draw_Poly_Lines, "DrawPolyLines");

   procedure Draw_Poly_Lines_Ex
     (center     : Rayda_Types.Vector2;
      sides      : Interfaces.C.int;
      radius     : Interfaces.C.C_float;
      rotation   : Interfaces.C.C_float;
      line_thick : Interfaces.C.C_float;
      color      : Rayda_Types.Color);
   pragma Import (C, Draw_Poly_Lines_Ex, "DrawPolyLinesEx");

   procedure Draw_Ellipse
     (center_x, center_y : Interfaces.C.int;
      radius_h, radius_v : Interfaces.C.C_float;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Ellipse, "DrawEllipse");

   procedure Draw_Ellipse_Lines
     (center_x, center_y : Interfaces.C.int;
      radius_h, radius_v : Interfaces.C.C_float;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Ellipse_Lines, "DrawEllipseLines");

   function Load_Texture
     (file_name : Interfaces.C.char_array) return Rayda_Types.Texture;
   pragma Import (C, Load_Texture, "LoadTexture");

   procedure Unload_Texture (texture : Rayda_Types.Texture);
   pragma Import (C, Unload_Texture, "UnloadTexture");

   procedure Draw_Texture
     (texture      : Rayda_Types.Texture;
      pos_x, pos_y : Interfaces.C.int;
      tint         : Rayda_Types.Color);
   pragma Import (C, Draw_Texture, "DrawTexture");

   procedure Draw_Texture_V
     (texture  : Rayda_Types.Texture;
      position : Rayda_Types.Vector2;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Texture_V, "DrawTextureV");

   procedure Draw_Texture_Ex
     (texture  : Rayda_Types.Texture;
      position : Rayda_Types.Vector2;
      rotation : Interfaces.C.C_float;
      scale    : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Texture_Ex, "DrawTextureEx");

   procedure Draw_Texture_Rec
     (texture  : Rayda_Types.Texture;
      source   : Rayda_Types.Rectangle;
      position : Rayda_Types.Vector2;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Texture_Rec, "DrawTextureRec");

   procedure Draw_Texture_Pro
     (texture      : Rayda_Types.Texture;
      source, dest : Rayda_Types.Rectangle;
      origin       : Rayda_Types.Vector2;
      rotation     : Interfaces.C.C_float;
      tint         : Rayda_Types.Color);
   pragma Import (C, Draw_Texture_Pro, "DrawTexturePro");

   procedure Draw_Text
     (text                    : Interfaces.C.char_array;
      pos_x, pos_y, font_size : Interfaces.C.int;
      color                   : Rayda_Types.Color);
   pragma Import (C, Draw_Text, "DrawText");

   procedure Draw_Text_Ex
     (font               : Rayda_Types.Font;
      text               : Interfaces.C.char_array;
      position           : Rayda_Types.Vector2;
      font_size, spacing : Interfaces.C.C_float;
      tint               : Rayda_Types.Color);
   pragma Import (C, Draw_Text_Ex, "DrawTextEx");

   procedure Draw_Text_Pro
     (font               : Rayda_Types.Font;
      text               : Interfaces.C.char_array;
      position           : Rayda_Types.Vector2;
      origin             : Rayda_Types.Vector2;
      rotation           : Interfaces.C.C_float;
      font_size, spacing : Interfaces.C.C_float;
      tint               : Rayda_Types.Color);
   pragma Import (C, Draw_Text_Pro, "DrawTextPro");

   function Measure_Text
     (text : Interfaces.C.char_array; font_size : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Measure_Text, "MeasureText");

   function Measure_Text_Ex
     (font               : Rayda_Types.Font;
      text               : Interfaces.C.char_array;
      font_size, spacing : Interfaces.C.C_float) return Rayda_Types.Vector2;
   pragma Import (C, Measure_Text_Ex, "MeasureTextEx");

   -- Font loading
   function Load_Font
     (file_name : Interfaces.C.char_array) return Rayda_Types.Font;
   pragma Import (C, Load_Font, "LoadFont");

   function Load_Font_Ex
     (file_name  : Interfaces.C.char_array;
      font_size  : Interfaces.C.int;
      font_chars : access Interfaces.C.int;
      glyph_count : Interfaces.C.int) return Rayda_Types.Font;
   pragma Import (C, Load_Font_Ex, "LoadFontEx");

   function Get_Font_Default return Rayda_Types.Font;
   pragma Import (C, Get_Font_Default, "GetFontDefault");

   procedure Unload_Font (font : Rayda_Types.Font);
   pragma Import (C, Unload_Font, "UnloadFont");

   -- Input-related functions: keyboard
   function Is_Key_Pressed (key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Pressed, "IsKeyPressed");

   function Is_Key_Down (key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Down, "IsKeyDown");

   function Is_Key_Released (key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Released, "IsKeyReleased");

   function Is_Key_Up (key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Up, "IsKeyUp");

   procedure Set_Exit_Key (key : Interfaces.C.int);
   pragma Import (C, Set_Exit_Key, "SetExitKey");

   function Get_Key_Pressed return Interfaces.C.int;
   pragma Import (C, Get_Key_Pressed, "GetKeyPressed");

   function Get_Char_Pressed return Interfaces.C.int;
   pragma Import (C, Get_Char_Pressed, "GetCharPressed");

   -- Input-related functions: mouse
   function Is_Mouse_Button_Pressed
     (button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Mouse_Button_Pressed, "IsMouseButtonPressed");

   function Is_Mouse_Button_Down
     (button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Mouse_Button_Down, "IsMouseButtonDown");

   function Is_Mouse_Button_Released
     (button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Mouse_Button_Released, "IsMouseButtonReleased");

   function Is_Mouse_Button_Up
     (button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Mouse_Button_Up, "IsMouseButtonUp");

   function Get_Mouse_X return Interfaces.C.int;
   pragma Import (C, Get_Mouse_X, "GetMouseX");

   function Get_Mouse_Y return Interfaces.C.int;
   pragma Import (C, Get_Mouse_Y, "GetMouseY");

   function Get_Mouse_Position return Rayda_Types.Vector2;
   pragma Import (C, Get_Mouse_Position, "GetMousePosition");

   function Get_Mouse_Delta return Rayda_Types.Vector2;
   pragma Import (C, Get_Mouse_Delta, "GetMouseDelta");

   procedure Set_Mouse_Position (x, y : Interfaces.C.int);
   pragma Import (C, Set_Mouse_Position, "SetMousePosition");

   procedure Set_Mouse_Offset (offset_x, offset_y : Interfaces.C.int);
   pragma Import (C, Set_Mouse_Offset, "SetMouseOffset");

   procedure Set_Mouse_Scale (scale_x, scale_y : Interfaces.C.C_float);
   pragma Import (C, Set_Mouse_Scale, "SetMouseScale");

   function Get_Mouse_Wheel_Move return Interfaces.C.C_float;
   pragma Import (C, Get_Mouse_Wheel_Move, "GetMouseWheelMove");

   function Get_Mouse_Wheel_Move_V return Rayda_Types.Vector2;
   pragma Import (C, Get_Mouse_Wheel_Move_V, "GetMouseWheelMoveV");

   procedure Set_Mouse_Cursor (cursor : Interfaces.C.int);
   pragma Import (C, Set_Mouse_Cursor, "SetMouseCursor");

   -- Screen-space-related functions
   function Get_Screen_Width return Interfaces.C.int;
   pragma Import (C, Get_Screen_Width, "GetScreenWidth");

   function Get_Screen_Height return Interfaces.C.int;
   pragma Import (C, Get_Screen_Height, "GetScreenHeight");

   function Get_Render_Width return Interfaces.C.int;
   pragma Import (C, Get_Render_Width, "GetRenderWidth");

   function Get_Render_Height return Interfaces.C.int;
   pragma Import (C, Get_Render_Height, "GetRenderHeight");

   -- Camera System Functions (2D)
   procedure Begin_Mode2D (camera : Rayda_Types.Camera2D);
   pragma Import (C, Begin_Mode2D, "BeginMode2D");

   procedure End_Mode2D;
   pragma Import (C, End_Mode2D, "EndMode2D");

   function Get_Screen_To_World2D
     (position : Rayda_Types.Vector2;
      camera   : Rayda_Types.Camera2D) return Rayda_Types.Vector2;
   pragma Import (C, Get_Screen_To_World2D, "GetScreenToWorld2D");

   function Get_World_To_Screen2D
     (position : Rayda_Types.Vector2;
      camera   : Rayda_Types.Camera2D) return Rayda_Types.Vector2;
   pragma Import (C, Get_World_To_Screen2D, "GetWorldToScreen2D");

   -- Camera System Functions (3D)
   procedure Begin_Mode3D (camera : Rayda_Types.Camera3D);
   pragma Import (C, Begin_Mode3D, "BeginMode3D");

   procedure End_Mode3D;
   pragma Import (C, End_Mode3D, "EndMode3D");

   -- Collision detection functions
   function Check_Collision_Recs
     (rec1, rec2 : Rayda_Types.Rectangle) return Interfaces.C.int;
   pragma Import (C, Check_Collision_Recs, "CheckCollisionRecs");

   function Check_Collision_Circles
     (center1 : Rayda_Types.Vector2;
      radius1 : Interfaces.C.C_float;
      center2 : Rayda_Types.Vector2;
      radius2 : Interfaces.C.C_float) return Interfaces.C.int;
   pragma Import (C, Check_Collision_Circles, "CheckCollisionCircles");

   function Check_Collision_Circle_Rec
     (center : Rayda_Types.Vector2;
      radius : Interfaces.C.C_float;
      rec    : Rayda_Types.Rectangle) return Interfaces.C.int;
   pragma Import (C, Check_Collision_Circle_Rec, "CheckCollisionCircleRec");

   function Check_Collision_Point_Rec
     (point : Rayda_Types.Vector2;
      rec   : Rayda_Types.Rectangle) return Interfaces.C.int;
   pragma Import (C, Check_Collision_Point_Rec, "CheckCollisionPointRec");

   function Check_Collision_Point_Circle
     (point  : Rayda_Types.Vector2;
      center : Rayda_Types.Vector2;
      radius : Interfaces.C.C_float) return Interfaces.C.int;
   pragma Import (C, Check_Collision_Point_Circle, "CheckCollisionPointCircle");

   function Get_Collision_Rec
     (rec1, rec2 : Rayda_Types.Rectangle) return Rayda_Types.Rectangle;
   pragma Import (C, Get_Collision_Rec, "GetCollisionRec");

   -- Audio device management functions
   procedure Init_Audio_Device;
   pragma Import (C, Init_Audio_Device, "InitAudioDevice");

   procedure Close_Audio_Device;
   pragma Import (C, Close_Audio_Device, "CloseAudioDevice");

   function Is_Audio_Device_Ready return Interfaces.C.int;
   pragma Import (C, Is_Audio_Device_Ready, "IsAudioDeviceReady");

   procedure Set_Master_Volume (volume : Interfaces.C.C_float);
   pragma Import (C, Set_Master_Volume, "SetMasterVolume");

   -- Sound loading/unloading functions
   function Load_Sound
     (file_name : Interfaces.C.char_array) return Rayda_Types.Sound;
   pragma Import (C, Load_Sound, "LoadSound");

   procedure Unload_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Unload_Sound, "UnloadSound");

   -- Sound playing functions
   procedure Play_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Play_Sound, "PlaySound");

   procedure Stop_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Stop_Sound, "StopSound");

   procedure Pause_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Pause_Sound, "PauseSound");

   procedure Resume_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Resume_Sound, "ResumeSound");

   function Is_Sound_Playing (sound : Rayda_Types.Sound) return Interfaces.C.int;
   pragma Import (C, Is_Sound_Playing, "IsSoundPlaying");

   procedure Set_Sound_Volume
     (sound : Rayda_Types.Sound; volume : Interfaces.C.C_float);
   pragma Import (C, Set_Sound_Volume, "SetSoundVolume");

   procedure Set_Sound_Pitch
     (sound : Rayda_Types.Sound; pitch : Interfaces.C.C_float);
   pragma Import (C, Set_Sound_Pitch, "SetSoundPitch");

   procedure Set_Sound_Pan
     (sound : Rayda_Types.Sound; pan : Interfaces.C.C_float);
   pragma Import (C, Set_Sound_Pan, "SetSoundPan");

   -- Random values generation functions
   procedure Set_Random_Seed (seed : Interfaces.C.unsigned);
   pragma Import (C, Set_Random_Seed, "SetRandomSeed");

   function Get_Random_Value
     (min, max : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Random_Value, "GetRandomValue");

   -- Color-related functions
   function Fade
     (color : Rayda_Types.Color; alpha : Interfaces.C.C_float)
      return Rayda_Types.Color;
   pragma Import (C, Fade, "Fade");

   function Color_To_Int (color : Rayda_Types.Color) return Interfaces.C.int;
   pragma Import (C, Color_To_Int, "ColorToInt");

   function Color_Normalize
     (color : Rayda_Types.Color) return Rayda_Types.Vector4;
   pragma Import (C, Color_Normalize, "ColorNormalize");

   function Color_From_Normalized
     (normalized : Rayda_Types.Vector4) return Rayda_Types.Color;
   pragma Import (C, Color_From_Normalized, "ColorFromNormalized");

   function Color_To_HSV
     (color : Rayda_Types.Color) return Rayda_Types.Vector3;
   pragma Import (C, Color_To_HSV, "ColorToHSV");

   function Color_From_HSV
     (hue, saturation, value : Interfaces.C.C_float) return Rayda_Types.Color;
   pragma Import (C, Color_From_HSV, "ColorFromHSV");

   function Get_Color (hex_value : Interfaces.C.unsigned) return Rayda_Types.Color;
   pragma Import (C, Get_Color, "GetColor");

   -- File management functions
   function Load_File_Data
     (file_name   : Interfaces.C.char_array;
      bytes_read : access Interfaces.C.unsigned) return access Interfaces.C.unsigned_char;
   pragma Import (C, Load_File_Data, "LoadFileData");

   procedure Unload_File_Data (data : access Interfaces.C.unsigned_char);
   pragma Import (C, Unload_File_Data, "UnloadFileData");

   function Save_File_Data
     (file_name    : Interfaces.C.char_array;
      data        : access Interfaces.C.unsigned_char;
      bytes_to_write : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, Save_File_Data, "SaveFileData");

   function File_Exists (file_name : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, File_Exists, "FileExists");

   function Directory_Exists (dir_path : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Directory_Exists, "DirectoryExists");
end Rayda;
