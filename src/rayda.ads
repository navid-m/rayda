with Interfaces.C;
with Interfaces.C.Strings;
with Rayda_Types;
with System;

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
   use type Rayda_Types.Model;
   use type Rayda_Types.Music;
   use type Rayda_Types.Image;

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

   procedure Set_Target_FPS (fps : Interfaces.C.int);
   pragma Import (C, Set_Target_FPS, "SetTargetFPS");

   function Get_FPS return Interfaces.C.int;
   pragma Import (C, Get_FPS, "GetFPS");

   function Get_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Time, "GetTime");

   function Get_Frame_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Frame_Time, "GetFrameTime");

   procedure Begin_Drawing;
   pragma Import (C, Begin_Drawing, "BeginDrawing");

   procedure End_Drawing;
   pragma Import (C, End_Drawing, "EndDrawing");

   procedure Clear_Background (color : Rayda_Types.Color);
   pragma Import (C, Clear_Background, "ClearBackground");

   procedure Draw_Pixel
     (pos_x, pos_y : Interfaces.C.int; color : Rayda_Types.Color);
   pragma Import (C, Draw_Pixel, "DrawPixel");

   procedure Draw_Pixel_V
     (position : Rayda_Types.Vector2; color : Rayda_Types.Color);
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
     (center       : Rayda_Types.Vector2;
      inner_radius : Interfaces.C.C_float;
      outer_radius : Interfaces.C.C_float;
      start_angle  : Interfaces.C.C_float;
      end_angle    : Interfaces.C.C_float;
      segments     : Interfaces.C.int;
      color        : Rayda_Types.Color);
   pragma Import (C, Draw_Ring, "DrawRing");

   procedure Draw_Ring_Lines
     (center       : Rayda_Types.Vector2;
      inner_radius : Interfaces.C.C_float;
      outer_radius : Interfaces.C.C_float;
      start_angle  : Interfaces.C.C_float;
      end_angle    : Interfaces.C.C_float;
      segments     : Interfaces.C.int;
      color        : Rayda_Types.Color);
   pragma Import (C, Draw_Ring_Lines, "DrawRingLines");

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
     (rec        : Rayda_Types.Rectangle;
      line_thick : Interfaces.C.C_float;
      color      : Rayda_Types.Color);
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
   pragma
     Import (C, Draw_Rectangle_Rounded_Lines, "DrawRectangleRoundedLines");

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
     (rec : Rayda_Types.Rectangle; col1, col2, col3, col4 : Rayda_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient_Ex, "DrawRectangleGradientEx");

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

   function Load_Font
     (file_name : Interfaces.C.char_array) return Rayda_Types.Font;
   pragma Import (C, Load_Font, "LoadFont");

   function Load_Font_Ex
     (file_name   : Interfaces.C.char_array;
      font_size   : Interfaces.C.int;
      font_chars  : access Interfaces.C.int;
      glyph_count : Interfaces.C.int) return Rayda_Types.Font;
   pragma Import (C, Load_Font_Ex, "LoadFontEx");

   function Get_Font_Default return Rayda_Types.Font;
   pragma Import (C, Get_Font_Default, "GetFontDefault");

   procedure Unload_Font (font : Rayda_Types.Font);
   pragma Import (C, Unload_Font, "UnloadFont");

   procedure Draw_FPS (pos_x, pos_y : Interfaces.C.int);
   pragma Import (C, Draw_FPS, "DrawFPS");

   procedure Draw_Text_Codepoint
     (font      : Rayda_Types.Font;
      codepoint : Interfaces.C.int;
      position  : Rayda_Types.Vector2;
      font_size : Interfaces.C.C_float;
      tint      : Rayda_Types.Color);
   pragma Import (C, Draw_Text_Codepoint, "DrawTextCodepoint");

   procedure Draw_Text_Codepoints
     (font               : Rayda_Types.Font;
      codepoints         : access Interfaces.C.int;
      codepoint_count    : Interfaces.C.int;
      position           : Rayda_Types.Vector2;
      font_size, spacing : Interfaces.C.C_float;
      tint               : Rayda_Types.Color);
   pragma Import (C, Draw_Text_Codepoints, "DrawTextCodepoints");

   procedure Set_Text_Line_Spacing (spacing : Interfaces.C.int);
   pragma Import (C, Set_Text_Line_Spacing, "SetTextLineSpacing");

   function Get_Glyph_Index
     (font : Rayda_Types.Font; codepoint : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Get_Glyph_Index, "GetGlyphIndex");

   function Get_Glyph_Info
     (font : Rayda_Types.Font; codepoint : Interfaces.C.int)
      return Rayda_Types.Glyph_Info;
   pragma Import (C, Get_Glyph_Info, "GetGlyphInfo");

   function Get_Glyph_Atlas_Rec
     (font : Rayda_Types.Font; codepoint : Interfaces.C.int)
      return Rayda_Types.Rectangle;
   pragma Import (C, Get_Glyph_Atlas_Rec, "GetGlyphAtlasRec");

   function Load_UTF8
     (codepoints : access Interfaces.C.int; length : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Load_UTF8, "LoadUTF8");

   procedure Unload_UTF8 (text : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Unload_UTF8, "UnloadUTF8");

   function Load_Codepoints
     (text : Interfaces.C.char_array; count : access Interfaces.C.int)
      return access Interfaces.C.int;
   pragma Import (C, Load_Codepoints, "LoadCodepoints");

   procedure Unload_Codepoints (codepoints : access Interfaces.C.int);
   pragma Import (C, Unload_Codepoints, "UnloadCodepoints");

   function Get_Codepoint_Count
     (text : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Get_Codepoint_Count, "GetCodepointCount");

   function Get_Codepoint
     (text : Interfaces.C.char_array; codepoint_size : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Get_Codepoint, "GetCodepoint");

   function Get_Codepoint_Next
     (text : Interfaces.C.char_array; codepoint_size : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Get_Codepoint_Next, "GetCodepointNext");

   function Get_Codepoint_Previous
     (text : Interfaces.C.char_array; codepoint_size : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Get_Codepoint_Previous, "GetCodepointPrevious");

   function Codepoint_To_UTF8
     (codepoint : Interfaces.C.int; utf8_size : access Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Codepoint_To_UTF8, "CodepointToUTF8");

   function Text_Copy
     (dst, src : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Text_Copy, "TextCopy");

   function Text_Is_Equal
     (text1, text2 : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Text_Is_Equal, "TextIsEqual");

   function Text_Length
     (text : Interfaces.C.char_array) return Interfaces.C.unsigned;
   pragma Import (C, Text_Length, "TextLength");

   function Text_Format
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Format, "TextFormat");

   function Text_Subtext
     (text : Interfaces.C.char_array; position, length : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Subtext, "TextSubtext");

   function Text_Replace
     (text, replace, by : Interfaces.C.char_array)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Replace, "TextReplace");

   function Text_Insert
     (text, insert : Interfaces.C.char_array; position : Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Insert, "TextInsert");

   function Text_Join
     (text_list : access Interfaces.C.Strings.chars_ptr;
      count     : Interfaces.C.int;
      delimiter : Interfaces.C.char_array)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Join, "TextJoin");

   function Text_Split
     (text      : Interfaces.C.char_array;
      delimiter : Interfaces.C.char;
      count     : access Interfaces.C.int)
      return access Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_Split, "TextSplit");

   procedure Text_Append
     (text, append : Interfaces.C.char_array;
      position     : access Interfaces.C.int);
   pragma Import (C, Text_Append, "TextAppend");

   function Text_Find_Index
     (text, find : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Text_Find_Index, "TextFindIndex");

   function Text_To_Upper
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_To_Upper, "TextToUpper");

   function Text_To_Lower
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_To_Lower, "TextToLower");

   function Text_To_Pascal
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_To_Pascal, "TextToPascal");

   function Text_To_Snake
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_To_Snake, "TextToSnake");

   function Text_To_Camel
     (text : Interfaces.C.char_array) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Text_To_Camel, "TextToCamel");

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

   procedure Show_Cursor;
   pragma Import (C, Show_Cursor, "ShowCursor");

   procedure Hide_Cursor;
   pragma Import (C, Hide_Cursor, "HideCursor");

   function Is_Cursor_Hidden return Interfaces.C.int;
   pragma Import (C, Is_Cursor_Hidden, "IsCursorHidden");

   procedure Enable_Cursor;
   pragma Import (C, Enable_Cursor, "EnableCursor");

   procedure Disable_Cursor;
   pragma Import (C, Disable_Cursor, "DisableCursor");

   function Is_Cursor_On_Screen return Interfaces.C.int;
   pragma Import (C, Is_Cursor_On_Screen, "IsCursorOnScreen");

   function Get_Screen_Width return Interfaces.C.int;
   pragma Import (C, Get_Screen_Width, "GetScreenWidth");

   function Get_Screen_Height return Interfaces.C.int;
   pragma Import (C, Get_Screen_Height, "GetScreenHeight");

   function Get_Render_Width return Interfaces.C.int;
   pragma Import (C, Get_Render_Width, "GetRenderWidth");

   function Get_Render_Height return Interfaces.C.int;
   pragma Import (C, Get_Render_Height, "GetRenderHeight");

   procedure Begin_Mode2D (camera : Rayda_Types.Camera2D);
   pragma Import (C, Begin_Mode2D, "BeginMode2D");

   procedure End_Mode2D;
   pragma Import (C, End_Mode2D, "EndMode2D");

   function Get_Screen_To_World2D
     (position : Rayda_Types.Vector2; camera : Rayda_Types.Camera2D)
      return Rayda_Types.Vector2;
   pragma Import (C, Get_Screen_To_World2D, "GetScreenToWorld2D");

   function Get_World_To_Screen2D
     (position : Rayda_Types.Vector2; camera : Rayda_Types.Camera2D)
      return Rayda_Types.Vector2;
   pragma Import (C, Get_World_To_Screen2D, "GetWorldToScreen2D");

   procedure Begin_Mode3D (camera : Rayda_Types.Camera3D);
   pragma Import (C, Begin_Mode3D, "BeginMode3D");

   procedure End_Mode3D;
   pragma Import (C, End_Mode3D, "EndMode3D");

   procedure Draw_Cube
     (position              : Rayda_Types.Vector3;
      width, height, length : Interfaces.C.C_float;
      color                 : Rayda_Types.Color);
   pragma Import (C, Draw_Cube, "DrawCube");

   procedure Draw_Cube_Wires
     (position              : Rayda_Types.Vector3;
      width, height, length : Interfaces.C.C_float;
      color                 : Rayda_Types.Color);
   pragma Import (C, Draw_Cube_Wires, "DrawCubeWires");

   procedure Draw_Sphere
     (center_pos : Rayda_Types.Vector3;
      radius     : Interfaces.C.C_float;
      color      : Rayda_Types.Color);
   pragma Import (C, Draw_Sphere, "DrawSphere");

   procedure Draw_Sphere_Wires
     (center_pos    : Rayda_Types.Vector3;
      radius        : Interfaces.C.C_float;
      rings, slices : Interfaces.C.int;
      color         : Rayda_Types.Color);
   pragma Import (C, Draw_Sphere_Wires, "DrawSphereWires");

   procedure Draw_Cylinder
     (position                  : Rayda_Types.Vector3;
      radius_top, radius_bottom : Interfaces.C.C_float;
      height                    : Interfaces.C.C_float;
      slices                    : Interfaces.C.int;
      color                     : Rayda_Types.Color);
   pragma Import (C, Draw_Cylinder, "DrawCylinder");

   procedure Draw_Cylinder_Wires
     (position                  : Rayda_Types.Vector3;
      radius_top, radius_bottom : Interfaces.C.C_float;
      height                    : Interfaces.C.C_float;
      slices                    : Interfaces.C.int;
      color                     : Rayda_Types.Color);
   pragma Import (C, Draw_Cylinder_Wires, "DrawCylinderWires");

   procedure Draw_Grid
     (slices : Interfaces.C.int; spacing : Interfaces.C.C_float);
   pragma Import (C, Draw_Grid, "DrawGrid");

   function Load_Model
     (file_name : Interfaces.C.char_array) return Rayda_Types.Model;
   pragma Import (C, Load_Model, "LoadModel");

   procedure Unload_Model (model : Rayda_Types.Model);
   pragma Import (C, Unload_Model, "UnloadModel");

   procedure Draw_Model
     (model    : Rayda_Types.Model;
      position : Rayda_Types.Vector3;
      scale    : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Model, "DrawModel");

   procedure Draw_Model_Ex
     (model          : Rayda_Types.Model;
      position       : Rayda_Types.Vector3;
      rotation_axis  : Rayda_Types.Vector3;
      rotation_angle : Interfaces.C.C_float;
      scale          : Rayda_Types.Vector3;
      tint           : Rayda_Types.Color);
   pragma Import (C, Draw_Model_Ex, "DrawModelEx");

   procedure Draw_Model_Wires
     (model    : Rayda_Types.Model;
      position : Rayda_Types.Vector3;
      scale    : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Model_Wires, "DrawModelWires");

   procedure Draw_Model_Wires_Ex
     (model          : Rayda_Types.Model;
      position       : Rayda_Types.Vector3;
      rotation_axis  : Rayda_Types.Vector3;
      rotation_angle : Interfaces.C.C_float;
      scale          : Rayda_Types.Vector3;
      tint           : Rayda_Types.Color);
   pragma Import (C, Draw_Model_Wires_Ex, "DrawModelWiresEx");

   procedure Draw_Model_Points
     (model    : Rayda_Types.Model;
      position : Rayda_Types.Vector3;
      scale    : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Model_Points, "DrawModelPoints");

   procedure Draw_Model_Points_Ex
     (model          : Rayda_Types.Model;
      position       : Rayda_Types.Vector3;
      rotation_axis  : Rayda_Types.Vector3;
      rotation_angle : Interfaces.C.C_float;
      scale          : Rayda_Types.Vector3;
      tint           : Rayda_Types.Color);
   pragma Import (C, Draw_Model_Points_Ex, "DrawModelPointsEx");

   procedure Draw_Bounding_Box
     (box : Rayda_Types.Bounding_Box; color : Rayda_Types.Color);
   pragma Import (C, Draw_Bounding_Box, "DrawBoundingBox");

   procedure Draw_Billboard
     (camera   : Rayda_Types.Camera3D;
      texture  : Rayda_Types.Texture2D;
      position : Rayda_Types.Vector3;
      size     : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Billboard, "DrawBillboard");

   procedure Draw_Billboard_Rec
     (camera   : Rayda_Types.Camera3D;
      texture  : Rayda_Types.Texture2D;
      source   : Rayda_Types.Rectangle;
      position : Rayda_Types.Vector3;
      size     : Rayda_Types.Vector2;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Billboard_Rec, "DrawBillboardRec");

   procedure Draw_Billboard_Pro
     (camera   : Rayda_Types.Camera3D;
      texture  : Rayda_Types.Texture2D;
      source   : Rayda_Types.Rectangle;
      position : Rayda_Types.Vector3;
      up       : Rayda_Types.Vector3;
      size     : Rayda_Types.Vector2;
      origin   : Rayda_Types.Vector2;
      rotation : Interfaces.C.C_float;
      tint     : Rayda_Types.Color);
   pragma Import (C, Draw_Billboard_Pro, "DrawBillboardPro");

   procedure Draw_Line3D
     (start_pos, end_pos : Rayda_Types.Vector3; color : Rayda_Types.Color);
   pragma Import (C, Draw_Line3D, "DrawLine3D");

   procedure Draw_Point3D
     (position : Rayda_Types.Vector3; color : Rayda_Types.Color);
   pragma Import (C, Draw_Point3D, "DrawPoint3D");

   procedure Draw_Circle3D
     (center         : Rayda_Types.Vector3;
      radius         : Interfaces.C.C_float;
      rotation_axis  : Rayda_Types.Vector3;
      rotation_angle : Interfaces.C.C_float;
      color          : Rayda_Types.Color);
   pragma Import (C, Draw_Circle3D, "DrawCircle3D");

   procedure Draw_Triangle3D
     (v1, v2, v3 : Rayda_Types.Vector3; color : Rayda_Types.Color);
   pragma Import (C, Draw_Triangle3D, "DrawTriangle3D");

   procedure Draw_Triangle_Strip3D
     (points      : access Rayda_Types.Vector3;
      point_count : Interfaces.C.int;
      color       : Rayda_Types.Color);
   pragma Import (C, Draw_Triangle_Strip3D, "DrawTriangleStrip3D");

   procedure Draw_Cube_V
     (position, size : Rayda_Types.Vector3; color : Rayda_Types.Color);
   pragma Import (C, Draw_Cube_V, "DrawCubeV");

   procedure Draw_Cube_Wires_V
     (position, size : Rayda_Types.Vector3; color : Rayda_Types.Color);
   pragma Import (C, Draw_Cube_Wires_V, "DrawCubeWiresV");

   procedure Draw_Sphere_Ex
     (center_pos    : Rayda_Types.Vector3;
      radius        : Interfaces.C.C_float;
      rings, slices : Interfaces.C.int;
      color         : Rayda_Types.Color);
   pragma Import (C, Draw_Sphere_Ex, "DrawSphereEx");

   procedure Draw_Cylinder_Ex
     (start_pos, end_pos       : Rayda_Types.Vector3;
      start_radius, end_radius : Interfaces.C.C_float;
      sides                    : Interfaces.C.int;
      color                    : Rayda_Types.Color);
   pragma Import (C, Draw_Cylinder_Ex, "DrawCylinderEx");

   procedure Draw_Cylinder_Wires_Ex
     (start_pos, end_pos       : Rayda_Types.Vector3;
      start_radius, end_radius : Interfaces.C.C_float;
      sides                    : Interfaces.C.int;
      color                    : Rayda_Types.Color);
   pragma Import (C, Draw_Cylinder_Wires_Ex, "DrawCylinderWiresEx");

   procedure Draw_Capsule
     (start_pos, end_pos : Rayda_Types.Vector3;
      radius             : Interfaces.C.C_float;
      slices, rings      : Interfaces.C.int;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Capsule, "DrawCapsule");

   procedure Draw_Capsule_Wires
     (start_pos, end_pos : Rayda_Types.Vector3;
      radius             : Interfaces.C.C_float;
      slices, rings      : Interfaces.C.int;
      color              : Rayda_Types.Color);
   pragma Import (C, Draw_Capsule_Wires, "DrawCapsuleWires");

   procedure Draw_Plane
     (center_pos : Rayda_Types.Vector3;
      size       : Rayda_Types.Vector2;
      color      : Rayda_Types.Color);
   pragma Import (C, Draw_Plane, "DrawPlane");

   procedure Draw_Ray (ray : Rayda_Types.Ray; color : Rayda_Types.Color);
   pragma Import (C, Draw_Ray, "DrawRay");

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
     (point : Rayda_Types.Vector2; rec : Rayda_Types.Rectangle)
      return Interfaces.C.int;
   pragma Import (C, Check_Collision_Point_Rec, "CheckCollisionPointRec");

   function Check_Collision_Point_Circle
     (point  : Rayda_Types.Vector2;
      center : Rayda_Types.Vector2;
      radius : Interfaces.C.C_float) return Interfaces.C.int;
   pragma
     Import (C, Check_Collision_Point_Circle, "CheckCollisionPointCircle");

   function Get_Collision_Rec
     (rec1, rec2 : Rayda_Types.Rectangle) return Rayda_Types.Rectangle;
   pragma Import (C, Get_Collision_Rec, "GetCollisionRec");

   procedure Init_Audio_Device;
   pragma Import (C, Init_Audio_Device, "InitAudioDevice");

   procedure Close_Audio_Device;
   pragma Import (C, Close_Audio_Device, "CloseAudioDevice");

   function Is_Audio_Device_Ready return Interfaces.C.int;
   pragma Import (C, Is_Audio_Device_Ready, "IsAudioDeviceReady");

   procedure Set_Master_Volume (volume : Interfaces.C.C_float);
   pragma Import (C, Set_Master_Volume, "SetMasterVolume");

   function Load_Sound
     (file_name : Interfaces.C.char_array) return Rayda_Types.Sound;
   pragma Import (C, Load_Sound, "LoadSound");

   procedure Unload_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Unload_Sound, "UnloadSound");

   procedure Play_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Play_Sound, "PlaySound");

   procedure Stop_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Stop_Sound, "StopSound");

   procedure Pause_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Pause_Sound, "PauseSound");

   procedure Resume_Sound (sound : Rayda_Types.Sound);
   pragma Import (C, Resume_Sound, "ResumeSound");

   function Is_Sound_Playing
     (sound : Rayda_Types.Sound) return Interfaces.C.int;
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

   procedure Set_Random_Seed (seed : Interfaces.C.unsigned);
   pragma Import (C, Set_Random_Seed, "SetRandomSeed");

   function Get_Random_Value
     (min, max : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Random_Value, "GetRandomValue");

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

   function Get_Color
     (hex_value : Interfaces.C.unsigned) return Rayda_Types.Color;
   pragma Import (C, Get_Color, "GetColor");

   function Load_File_Data
     (file_name  : Interfaces.C.char_array;
      bytes_read : access Interfaces.C.unsigned)
      return access Interfaces.C.unsigned_char;
   pragma Import (C, Load_File_Data, "LoadFileData");

   procedure Unload_File_Data (data : access Interfaces.C.unsigned_char);
   pragma Import (C, Unload_File_Data, "UnloadFileData");

   function Save_File_Data
     (file_name      : Interfaces.C.char_array;
      data           : access Interfaces.C.unsigned_char;
      bytes_to_write : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, Save_File_Data, "SaveFileData");

   function File_Exists
     (file_name : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, File_Exists, "FileExists");

   function Directory_Exists
     (dir_path : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Directory_Exists, "DirectoryExists");

   function Load_Music_Stream
     (file_name : Interfaces.C.char_array) return Rayda_Types.Music;
   pragma Import (C, Load_Music_Stream, "LoadMusicStream");

   procedure Unload_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Unload_Music_Stream, "UnloadMusicStream");

   procedure Play_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Play_Music_Stream, "PlayMusicStream");

   procedure Update_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Update_Music_Stream, "UpdateMusicStream");

   procedure Stop_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Stop_Music_Stream, "StopMusicStream");

   procedure Pause_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Pause_Music_Stream, "PauseMusicStream");

   procedure Resume_Music_Stream (music : Rayda_Types.Music);
   pragma Import (C, Resume_Music_Stream, "ResumeMusicStream");

   function Is_Music_Stream_Playing
     (music : Rayda_Types.Music) return Interfaces.C.int;
   pragma Import (C, Is_Music_Stream_Playing, "IsMusicStreamPlaying");

   procedure Set_Music_Volume
     (music : Rayda_Types.Music; volume : Interfaces.C.C_float);
   pragma Import (C, Set_Music_Volume, "SetMusicVolume");

   procedure Set_Music_Pitch
     (music : Rayda_Types.Music; pitch : Interfaces.C.C_float);
   pragma Import (C, Set_Music_Pitch, "SetMusicPitch");

   procedure Set_Music_Pan
     (music : Rayda_Types.Music; pan : Interfaces.C.C_float);
   pragma Import (C, Set_Music_Pan, "SetMusicPan");

   function Get_Music_Time_Length
     (music : Rayda_Types.Music) return Interfaces.C.C_float;
   pragma Import (C, Get_Music_Time_Length, "GetMusicTimeLength");

   function Get_Music_Time_Played
     (music : Rayda_Types.Music) return Interfaces.C.C_float;
   pragma Import (C, Get_Music_Time_Played, "GetMusicTimePlayed");

   function Load_Image
     (file_name : Interfaces.C.char_array) return Rayda_Types.Image;
   pragma Import (C, Load_Image, "LoadImage");

   function Load_Image_Raw
     (file_name     : Interfaces.C.char_array;
      width, height : Interfaces.C.int;
      format        : Interfaces.C.int;
      header_size   : Interfaces.C.int) return Rayda_Types.Image;
   pragma Import (C, Load_Image_Raw, "LoadImageRaw");

   function Load_Image_SVG
     (file_name_or_data : Interfaces.C.char_array;
      width, height     : Interfaces.C.int) return Rayda_Types.Image;
   pragma Import (C, Load_Image_SVG, "LoadImageSvg");

   function Load_Image_Anim
     (file_name : Interfaces.C.char_array; frames : access Interfaces.C.int)
      return Rayda_Types.Image;
   pragma Import (C, Load_Image_Anim, "LoadImageAnim");

   function Load_Image_From_Memory
     (file_type : Interfaces.C.char_array;
      file_data : access Interfaces.C.unsigned_char;
      data_size : Interfaces.C.int) return Rayda_Types.Image;
   pragma Import (C, Load_Image_From_Memory, "LoadImageFromMemory");

   procedure Unload_Image (image : Rayda_Types.Image);
   pragma Import (C, Unload_Image, "UnloadImage");

   function Export_Image
     (image : Rayda_Types.Image; file_name : Interfaces.C.char_array)
      return Interfaces.C.int;
   pragma Import (C, Export_Image, "ExportImage");

   function Export_Image_To_Memory
     (image     : Rayda_Types.Image;
      file_type : Interfaces.C.char_array;
      file_size : access Interfaces.C.int)
      return access Interfaces.C.unsigned_char;
   pragma Import (C, Export_Image_To_Memory, "ExportImageToMemory");

   function Image_Copy (image : Rayda_Types.Image) return Rayda_Types.Image;
   pragma Import (C, Image_Copy, "ImageCopy");

   function Image_From_Image
     (image : Rayda_Types.Image; rec : Rayda_Types.Rectangle)
      return Rayda_Types.Image;
   pragma Import (C, Image_From_Image, "ImageFromImage");

   function Image_From_Channel
     (image : Rayda_Types.Image; selected_channel : Interfaces.C.int)
      return Rayda_Types.Image;
   pragma Import (C, Image_From_Channel, "ImageFromChannel");

   function Image_Text
     (text      : Interfaces.C.char_array;
      font_size : Interfaces.C.int;
      color     : Rayda_Types.Color) return Rayda_Types.Image;
   pragma Import (C, Image_Text, "ImageText");

   function Image_Text_Ex
     (font               : Rayda_Types.Font;
      text               : Interfaces.C.char_array;
      font_size, spacing : Interfaces.C.C_float;
      tint               : Rayda_Types.Color) return Rayda_Types.Image;
   pragma Import (C, Image_Text_Ex, "ImageTextEx");

   procedure Image_Format
     (image : access Rayda_Types.Image; new_format : Interfaces.C.int);
   pragma Import (C, Image_Format, "ImageFormat");

   procedure Image_To_POT
     (image : access Rayda_Types.Image; fill : Rayda_Types.Color);
   pragma Import (C, Image_To_POT, "ImageToPOT");

   procedure Image_Crop
     (image : access Rayda_Types.Image; crop : Rayda_Types.Rectangle);
   pragma Import (C, Image_Crop, "ImageCrop");

   procedure Image_Alpha_Crop
     (image : access Rayda_Types.Image; threshold : Interfaces.C.C_float);
   pragma Import (C, Image_Alpha_Crop, "ImageAlphaCrop");

   procedure Image_Alpha_Clear
     (image     : access Rayda_Types.Image;
      color     : Rayda_Types.Color;
      threshold : Interfaces.C.C_float);
   pragma Import (C, Image_Alpha_Clear, "ImageAlphaClear");

   procedure Image_Alpha_Mask
     (image : access Rayda_Types.Image; alpha_mask : Rayda_Types.Image);
   pragma Import (C, Image_Alpha_Mask, "ImageAlphaMask");

   procedure Image_Alpha_Premultiply (image : access Rayda_Types.Image);
   pragma Import (C, Image_Alpha_Premultiply, "ImageAlphaPremultiply");

   procedure Image_Blur_Gaussian
     (image : access Rayda_Types.Image; blur_size : Interfaces.C.int);
   pragma Import (C, Image_Blur_Gaussian, "ImageBlurGaussian");

   procedure Image_Kernel_Convolution
     (image       : access Rayda_Types.Image;
      kernel      : access Interfaces.C.C_float;
      kernel_size : Interfaces.C.int);
   pragma Import (C, Image_Kernel_Convolution, "ImageKernelConvolution");

   procedure Image_Resize
     (image                 : access Rayda_Types.Image;
      new_width, new_height : Interfaces.C.int);
   pragma Import (C, Image_Resize, "ImageResize");

   procedure Image_Resize_NN
     (image                 : access Rayda_Types.Image;
      new_width, new_height : Interfaces.C.int);
   pragma Import (C, Image_Resize_NN, "ImageResizeNN");

   procedure Image_Resize_Canvas
     (image                                     : access Rayda_Types.Image;
      new_width, new_height, offset_x, offset_y : Interfaces.C.int;
      fill                                      : Rayda_Types.Color);
   pragma Import (C, Image_Resize_Canvas, "ImageResizeCanvas");

   procedure Image_Mipmaps (image : access Rayda_Types.Image);
   pragma Import (C, Image_Mipmaps, "ImageMipmaps");

   procedure Image_Dither
     (image                      : access Rayda_Types.Image;
      r_bpp, g_bpp, b_bpp, a_bpp : Interfaces.C.int);
   pragma Import (C, Image_Dither, "ImageDither");

   procedure Image_Flip_Vertical (image : access Rayda_Types.Image);
   pragma Import (C, Image_Flip_Vertical, "ImageFlipVertical");

   procedure Image_Flip_Horizontal (image : access Rayda_Types.Image);
   pragma Import (C, Image_Flip_Horizontal, "ImageFlipHorizontal");

   procedure Image_Rotate
     (image : access Rayda_Types.Image; degrees : Interfaces.C.int);
   pragma Import (C, Image_Rotate, "ImageRotate");

   procedure Image_Rotate_CW (image : access Rayda_Types.Image);
   pragma Import (C, Image_Rotate_CW, "ImageRotateCW");

   procedure Image_Rotate_CCW (image : access Rayda_Types.Image);
   pragma Import (C, Image_Rotate_CCW, "ImageRotateCCW");

   procedure Image_Color_Tint
     (image : access Rayda_Types.Image; color : Rayda_Types.Color);
   pragma Import (C, Image_Color_Tint, "ImageColorTint");

   procedure Image_Color_Invert (image : access Rayda_Types.Image);
   pragma Import (C, Image_Color_Invert, "ImageColorInvert");

   procedure Image_Color_Grayscale (image : access Rayda_Types.Image);
   pragma Import (C, Image_Color_Grayscale, "ImageColorGrayscale");

   procedure Image_Color_Contrast
     (image : access Rayda_Types.Image; contrast : Interfaces.C.C_float);
   pragma Import (C, Image_Color_Contrast, "ImageColorContrast");

   procedure Image_Color_Brightness
     (image : access Rayda_Types.Image; brightness : Interfaces.C.int);
   pragma Import (C, Image_Color_Brightness, "ImageColorBrightness");

   procedure Image_Color_Replace
     (image : access Rayda_Types.Image; color, replace : Rayda_Types.Color);
   pragma Import (C, Image_Color_Replace, "ImageColorReplace");

   function Load_Image_Colors
     (image : Rayda_Types.Image) return access Rayda_Types.Color;
   pragma Import (C, Load_Image_Colors, "LoadImageColors");

   function Load_Image_Palette
     (image            : Rayda_Types.Image;
      max_palette_size : Interfaces.C.int;
      color_count      : access Interfaces.C.int)
      return access Rayda_Types.Color;
   pragma Import (C, Load_Image_Palette, "LoadImagePalette");

   procedure Unload_Image_Colors (colors : access Rayda_Types.Color);
   pragma Import (C, Unload_Image_Colors, "UnloadImageColors");

   procedure Unload_Image_Palette (colors : access Rayda_Types.Color);
   pragma Import (C, Unload_Image_Palette, "UnloadImagePalette");

   function Get_Image_Alpha_Border
     (image : Rayda_Types.Image; threshold : Interfaces.C.C_float)
      return Rayda_Types.Rectangle;
   pragma Import (C, Get_Image_Alpha_Border, "GetImageAlphaBorder");

   function Get_Image_Color
     (image : Rayda_Types.Image; x, y : Interfaces.C.int)
      return Rayda_Types.Color;
   pragma Import (C, Get_Image_Color, "GetImageColor");

   procedure Image_Clear_Background
     (dst : access Rayda_Types.Image; color : Rayda_Types.Color);
   pragma Import (C, Image_Clear_Background, "ImageClearBackground");

   procedure Image_Draw_Pixel
     (dst          : access Rayda_Types.Image;
      pos_x, pos_y : Interfaces.C.int;
      color        : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Pixel, "ImageDrawPixel");

   procedure Image_Draw_Pixel_V
     (dst      : access Rayda_Types.Image;
      position : Rayda_Types.Vector2;
      color    : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Pixel_V, "ImageDrawPixelV");

   procedure Image_Draw_Line
     (dst                                            :
        access Rayda_Types.Image;
      start_pos_x, start_pos_y, end_pos_x, end_pos_y : Interfaces.C.int;
      color                                          : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Line, "ImageDrawLine");

   procedure Image_Draw_Line_V
     (dst    : access Rayda_Types.Image;
      start  : Rayda_Types.Vector2;
      ending : Rayda_Types.Vector2;
      color  : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Line_V, "ImageDrawLineV");

   procedure Image_Draw_Line_Ex
     (dst    : access Rayda_Types.Image;
      start  : Rayda_Types.Vector2;
      ending : Rayda_Types.Vector2;
      thick  : Interfaces.C.int;
      color  : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Line_Ex, "ImageDrawLineEx");

   procedure Image_Draw_Circle
     (dst                        : access Rayda_Types.Image;
      center_x, center_y, radius : Interfaces.C.int;
      color                      : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Circle, "ImageDrawCircle");

   procedure Image_Draw_Circle_V
     (dst    : access Rayda_Types.Image;
      center : Rayda_Types.Vector2;
      radius : Interfaces.C.int;
      color  : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Circle_V, "ImageDrawCircleV");

   procedure Image_Draw_Circle_Lines
     (dst                        : access Rayda_Types.Image;
      center_x, center_y, radius : Interfaces.C.int;
      color                      : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Circle_Lines, "ImageDrawCircleLines");

   procedure Image_Draw_Circle_Lines_V
     (dst    : access Rayda_Types.Image;
      center : Rayda_Types.Vector2;
      radius : Interfaces.C.int;
      color  : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Circle_Lines_V, "ImageDrawCircleLinesV");

   procedure Image_Draw_Rectangle
     (dst                         : access Rayda_Types.Image;
      pos_x, pos_y, width, height : Interfaces.C.int;
      color                       : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Rectangle, "ImageDrawRectangle");

   procedure Image_Draw_Rectangle_V
     (dst            : access Rayda_Types.Image;
      position, size : Rayda_Types.Vector2;
      color          : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Rectangle_V, "ImageDrawRectangleV");

   procedure Image_Draw_Rectangle_Rec
     (dst   : access Rayda_Types.Image;
      rec   : Rayda_Types.Rectangle;
      color : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Rectangle_Rec, "ImageDrawRectangleRec");

   procedure Image_Draw_Rectangle_Lines
     (dst   : access Rayda_Types.Image;
      rec   : Rayda_Types.Rectangle;
      thick : Interfaces.C.int;
      color : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Rectangle_Lines, "ImageDrawRectangleLines");

   procedure Image_Draw_Triangle
     (dst        : access Rayda_Types.Image;
      v1, v2, v3 : Rayda_Types.Vector2;
      color      : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Triangle, "ImageDrawTriangle");

   procedure Image_Draw_Triangle_Ex
     (dst        : access Rayda_Types.Image;
      v1, v2, v3 : Rayda_Types.Vector2;
      c1, c2, c3 : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Triangle_Ex, "ImageDrawTriangleEx");

   procedure Image_Draw_Triangle_Lines
     (dst        : access Rayda_Types.Image;
      v1, v2, v3 : Rayda_Types.Vector2;
      color      : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Triangle_Lines, "ImageDrawTriangleLines");

   procedure Image_Draw_Triangle_Fan
     (dst         : access Rayda_Types.Image;
      points      : access Rayda_Types.Vector2;
      point_count : Interfaces.C.int;
      color       : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Triangle_Fan, "ImageDrawTriangleFan");

   procedure Image_Draw_Triangle_Strip
     (dst         : access Rayda_Types.Image;
      points      : access Rayda_Types.Vector2;
      point_count : Interfaces.C.int;
      color       : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Triangle_Strip, "ImageDrawTriangleStrip");

   procedure Image_Draw
     (dst              : access Rayda_Types.Image;
      src              : Rayda_Types.Image;
      src_rec, dst_rec : Rayda_Types.Rectangle;
      tint             : Rayda_Types.Color);
   pragma Import (C, Image_Draw, "ImageDraw");

   procedure Image_Draw_Text
     (dst                     : access Rayda_Types.Image;
      text                    : Interfaces.C.char_array;
      pos_x, pos_y, font_size : Interfaces.C.int;
      color                   : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Text, "ImageDrawText");

   procedure Image_Draw_Text_Ex
     (dst                : access Rayda_Types.Image;
      font               : Rayda_Types.Font;
      text               : Interfaces.C.char_array;
      position           : Rayda_Types.Vector2;
      font_size, spacing : Interfaces.C.C_float;
      tint               : Rayda_Types.Color);
   pragma Import (C, Image_Draw_Text_Ex, "ImageDrawTextEx");

   function Load_Automation_Event_List
     (file_name : Interfaces.C.char_array)
      return Rayda_Types.Automation_Event_List;
   pragma Import (C, Load_Automation_Event_List, "LoadAutomationEventList");

   procedure Unload_Automation_Event_List
     (list : Rayda_Types.Automation_Event_List);
   pragma
     Import (C, Unload_Automation_Event_List, "UnloadAutomationEventList");

   function Export_Automation_Event_List
     (list      : Rayda_Types.Automation_Event_List;
      file_name : Interfaces.C.char_array) return Interfaces.C.int;
   pragma
     Import (C, Export_Automation_Event_List, "ExportAutomationEventList");

   procedure Set_Automation_Event_List
     (list : access Rayda_Types.Automation_Event_List);
   pragma Import (C, Set_Automation_Event_List, "SetAutomationEventList");

   procedure Set_Automation_Event_Base_Frame (frame : Interfaces.C.int);
   pragma
     Import
       (C, Set_Automation_Event_Base_Frame, "SetAutomationEventBaseFrame");

   procedure Start_Automation_Event_Recording;
   pragma
     Import
       (C, Start_Automation_Event_Recording, "StartAutomationEventRecording");

   procedure Stop_Automation_Event_Recording;
   pragma
     Import
       (C, Stop_Automation_Event_Recording, "StopAutomationEventRecording");

   procedure Play_Automation_Event (event : Rayda_Types.Automation_Event);
   pragma Import (C, Play_Automation_Event, "PlayAutomationEvent");

   function Is_Key_Pressed_Repeat
     (key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Pressed_Repeat, "IsKeyPressedRepeat");

   function Is_Gamepad_Available
     (gamepad : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Gamepad_Available, "IsGamepadAvailable");

   function Get_Gamepad_Name
     (gamepad : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Gamepad_Name, "GetGamepadName");

   function Is_Gamepad_Button_Pressed
     (gamepad, button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Gamepad_Button_Pressed, "IsGamepadButtonPressed");

   function Is_Gamepad_Button_Down
     (gamepad, button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Gamepad_Button_Down, "IsGamepadButtonDown");

   function Is_Gamepad_Button_Released
     (gamepad, button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Gamepad_Button_Released, "IsGamepadButtonReleased");

   function Is_Gamepad_Button_Up
     (gamepad, button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Gamepad_Button_Up, "IsGamepadButtonUp");

   function Get_Gamepad_Button_Pressed return Interfaces.C.int;
   pragma Import (C, Get_Gamepad_Button_Pressed, "GetGamepadButtonPressed");

   function Get_Gamepad_Axis_Count
     (gamepad : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Gamepad_Axis_Count, "GetGamepadAxisCount");

   function Get_Gamepad_Axis_Movement
     (gamepad, axis : Interfaces.C.int) return Interfaces.C.C_float;
   pragma Import (C, Get_Gamepad_Axis_Movement, "GetGamepadAxisMovement");

   function Set_Gamepad_Mappings
     (mappings : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Set_Gamepad_Mappings, "SetGamepadMappings");

   procedure Set_Gamepad_Vibration
     (gamepad                 : Interfaces.C.int;
      left_motor, right_motor : Interfaces.C.C_float;
      duration                : Interfaces.C.C_float);
   pragma Import (C, Set_Gamepad_Vibration, "SetGamepadVibration");

   function Get_Touch_X return Interfaces.C.int;
   pragma Import (C, Get_Touch_X, "GetTouchX");

   function Get_Touch_Y return Interfaces.C.int;
   pragma Import (C, Get_Touch_Y, "GetTouchY");

   function Get_Touch_Position
     (index : Interfaces.C.int) return Rayda_Types.Vector2;
   pragma Import (C, Get_Touch_Position, "GetTouchPosition");

   function Get_Touch_Point_Id
     (index : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Touch_Point_Id, "GetTouchPointId");

   function Get_Touch_Point_Count return Interfaces.C.int;
   pragma Import (C, Get_Touch_Point_Count, "GetTouchPointCount");

   procedure Set_Gestures_Enabled (flags : Interfaces.C.unsigned);
   pragma Import (C, Set_Gestures_Enabled, "SetGesturesEnabled");

   function Is_Gesture_Detected
     (gesture : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, Is_Gesture_Detected, "IsGestureDetected");

   function Get_Gesture_Detected return Interfaces.C.int;
   pragma Import (C, Get_Gesture_Detected, "GetGestureDetected");

   function Get_Gesture_Hold_Duration return Interfaces.C.C_float;
   pragma Import (C, Get_Gesture_Hold_Duration, "GetGestureHoldDuration");

   function Get_Gesture_Drag_Vector return Rayda_Types.Vector2;
   pragma Import (C, Get_Gesture_Drag_Vector, "GetGestureDragVector");

   function Get_Gesture_Drag_Angle return Interfaces.C.C_float;
   pragma Import (C, Get_Gesture_Drag_Angle, "GetGestureDragAngle");

   function Get_Gesture_Pinch_Vector return Rayda_Types.Vector2;
   pragma Import (C, Get_Gesture_Pinch_Vector, "GetGesturePinchVector");

   function Get_Gesture_Pinch_Angle return Interfaces.C.C_float;
   pragma Import (C, Get_Gesture_Pinch_Angle, "GetGesturePinchAngle");

   procedure Swap_Screen_Buffer;
   pragma Import (C, Swap_Screen_Buffer, "SwapScreenBuffer");

   procedure Poll_Input_Events;
   pragma Import (C, Poll_Input_Events, "PollInputEvents");

   procedure Wait_Time (seconds : Interfaces.C.double);
   pragma Import (C, Wait_Time, "WaitTime");

   function Load_Random_Sequence
     (count, min, max : Interfaces.C.unsigned) return access Interfaces.C.int;
   pragma Import (C, Load_Random_Sequence, "LoadRandomSequence");

   procedure Unload_Random_Sequence (sequence : access Interfaces.C.int);
   pragma Import (C, Unload_Random_Sequence, "UnloadRandomSequence");

   procedure Take_Screenshot (file_name : Interfaces.C.char_array);
   pragma Import (C, Take_Screenshot, "TakeScreenshot");

   procedure Set_Config_Flags (flags : Interfaces.C.unsigned);
   pragma Import (C, Set_Config_Flags, "SetConfigFlags");

   procedure Open_URL (url : Interfaces.C.char_array);
   pragma Import (C, Open_URL, "OpenURL");

   procedure Trace_Log
     (log_level : Interfaces.C.int; text : Interfaces.C.char_array);
   pragma Import (C, Trace_Log, "TraceLog");

   procedure Set_Trace_Log_Level (log_level : Interfaces.C.int);
   pragma Import (C, Set_Trace_Log_Level, "SetTraceLogLevel");

   function Mem_Alloc (size : Interfaces.C.unsigned) return System.Address;
   pragma Import (C, Mem_Alloc, "MemAlloc");

   function Mem_Realloc
     (ptr : System.Address; size : Interfaces.C.unsigned)
      return System.Address;
   pragma Import (C, Mem_Realloc, "MemRealloc");

   procedure Mem_Free (ptr : System.Address);
   pragma Import (C, Mem_Free, "MemFree");

   function Export_Data_As_Code
     (data      : access Interfaces.C.unsigned_char;
      data_size : Interfaces.C.int;
      file_name : Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, Export_Data_As_Code, "ExportDataAsCode");

   function Load_File_Text
     (file_name : Interfaces.C.char_array)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Load_File_Text, "LoadFileText");

   procedure Unload_File_Text (text : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Unload_File_Text, "UnloadFileText");

   function Save_File_Text
     (file_name : Interfaces.C.char_array;
      text      : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, Save_File_Text, "SaveFileText");

   function Compress_Data
     (data           : access Interfaces.C.unsigned_char;
      data_size      : Interfaces.C.int;
      comp_data_size : access Interfaces.C.int)
      return access Interfaces.C.unsigned_char;
   pragma Import (C, Compress_Data, "CompressData");

   function Decompress_Data
     (comp_data      : access Interfaces.C.unsigned_char;
      comp_data_size : Interfaces.C.int;
      data_size      : access Interfaces.C.int)
      return access Interfaces.C.unsigned_char;
   pragma Import (C, Decompress_Data, "DecompressData");

   function Encode_Data_Base64
     (data        : access Interfaces.C.unsigned_char;
      data_size   : Interfaces.C.int;
      output_size : access Interfaces.C.int)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Encode_Data_Base64, "EncodeDataBase64");

   function Decode_Data_Base64
     (data        : access Interfaces.C.unsigned_char;
      output_size : access Interfaces.C.int)
      return access Interfaces.C.unsigned_char;
   pragma Import (C, Decode_Data_Base64, "DecodeDataBase64");

   function Compute_CRC32
     (data : access Interfaces.C.unsigned_char; data_size : Interfaces.C.int)
      return Interfaces.C.unsigned;
   pragma Import (C, Compute_CRC32, "ComputeCRC32");

   function Compute_MD5
     (data : access Interfaces.C.unsigned_char; data_size : Interfaces.C.int)
      return access Interfaces.C.unsigned;
   pragma Import (C, Compute_MD5, "ComputeMD5");

   function Compute_SHA1
     (data : access Interfaces.C.unsigned_char; data_size : Interfaces.C.int)
      return access Interfaces.C.unsigned;
   pragma Import (C, Compute_SHA1, "ComputeSHA1");

   procedure Begin_Texture_Mode (target : Rayda_Types.Render_Texture2D);
   pragma Import (C, Begin_Texture_Mode, "BeginTextureMode");

   procedure End_Texture_Mode;
   pragma Import (C, End_Texture_Mode, "EndTextureMode");

   procedure Begin_Shader_Mode (shader : Rayda_Types.Shader);
   pragma Import (C, Begin_Shader_Mode, "BeginShaderMode");

   procedure End_Shader_Mode;
   pragma Import (C, End_Shader_Mode, "EndShaderMode");

   procedure Begin_Blend_Mode (mode : Interfaces.C.int);
   pragma Import (C, Begin_Blend_Mode, "BeginBlendMode");

   procedure End_Blend_Mode;
   pragma Import (C, End_Blend_Mode, "EndBlendMode");

   procedure Begin_Scissor_Mode (x, y, width, height : Interfaces.C.int);
   pragma Import (C, Begin_Scissor_Mode, "BeginScissorMode");

   procedure End_Scissor_Mode;
   pragma Import (C, End_Scissor_Mode, "EndScissorMode");

   procedure Begin_Vr_Stereo_Mode (config : Rayda_Types.VR_Stereo_Config);
   pragma Import (C, Begin_Vr_Stereo_Mode, "BeginVrStereoMode");

   procedure End_Vr_Stereo_Mode;
   pragma Import (C, End_Vr_Stereo_Mode, "EndVrStereoMode");

   function Load_Shader
     (vs_file_name, fs_file_name : Interfaces.C.char_array)
      return Rayda_Types.Shader;
   pragma Import (C, Load_Shader, "LoadShader");

   function Load_Shader_From_Memory
     (vs_code, fs_code : Interfaces.C.char_array) return Rayda_Types.Shader;
   pragma Import (C, Load_Shader_From_Memory, "LoadShaderFromMemory");

   function Is_Shader_Valid
     (shader : Rayda_Types.Shader) return Interfaces.C.int;
   pragma Import (C, Is_Shader_Valid, "IsShaderValid");

   function Get_Shader_Location
     (shader : Rayda_Types.Shader; uniform_name : Interfaces.C.char_array)
      return Interfaces.C.int;
   pragma Import (C, Get_Shader_Location, "GetShaderLocation");

   function Get_Shader_Location_Attrib
     (shader : Rayda_Types.Shader; attrib_name : Interfaces.C.char_array)
      return Interfaces.C.int;
   pragma Import (C, Get_Shader_Location_Attrib, "GetShaderLocationAttrib");

   procedure Set_Shader_Value
     (shader       : Rayda_Types.Shader;
      loc_index    : Interfaces.C.int;
      value        : System.Address;
      uniform_type : Interfaces.C.int);
   pragma Import (C, Set_Shader_Value, "SetShaderValue");

   procedure Set_Shader_Value_V
     (shader       : Rayda_Types.Shader;
      loc_index    : Interfaces.C.int;
      value        : System.Address;
      uniform_type : Interfaces.C.int;
      count        : Interfaces.C.int);
   pragma Import (C, Set_Shader_Value_V, "SetShaderValueV");

   procedure Set_Shader_Value_Matrix
     (shader    : Rayda_Types.Shader;
      loc_index : Interfaces.C.int;
      mat       : Rayda_Types.Matrix);
   pragma Import (C, Set_Shader_Value_Matrix, "SetShaderValueMatrix");

   procedure Set_Shader_Value_Texture
     (shader    : Rayda_Types.Shader;
      loc_index : Interfaces.C.int;
      texture   : Rayda_Types.Texture2D);
   pragma Import (C, Set_Shader_Value_Texture, "SetShaderValueTexture");

   function Get_Screen_To_World_Ray
     (position : Rayda_Types.Vector2; camera : Rayda_Types.Camera3D)
      return Rayda_Types.Ray;
   pragma Import (C, Get_Screen_To_World_Ray, "GetScreenToWorldRay");

   function Get_Screen_To_World_Ray_Ex
     (position      : Rayda_Types.Vector2;
      camera        : Rayda_Types.Camera3D;
      width, height : Interfaces.C.int) return Rayda_Types.Ray;
   pragma Import (C, Get_Screen_To_World_Ray_Ex, "GetScreenToWorldRayEx");

   function Get_World_To_Screen
     (position : Rayda_Types.Vector3; camera : Rayda_Types.Camera3D)
      return Rayda_Types.Vector2;
   pragma Import (C, Get_World_To_Screen, "GetWorldToScreen");

   function Get_World_To_Screen_Ex
     (position      : Rayda_Types.Vector3;
      camera        : Rayda_Types.Camera3D;
      width, height : Interfaces.C.int) return Rayda_Types.Vector2;
   pragma Import (C, Get_World_To_Screen_Ex, "GetWorldToScreenEx");

   function Get_Camera_Matrix
     (camera : Rayda_Types.Camera3D) return Rayda_Types.Matrix;
   pragma Import (C, Get_Camera_Matrix, "GetCameraMatrix");

   function Get_Camera_Matrix2D
     (camera : Rayda_Types.Camera2D) return Rayda_Types.Matrix;
   pragma Import (C, Get_Camera_Matrix2D, "GetCameraMatrix2D");

   function Is_Window_State
     (flag : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, Is_Window_State, "IsWindowState");

   procedure Set_Window_State
     (flags : Interfaces.C.unsigned);
   pragma Import (C, Set_Window_State, "SetWindowState");

   procedure Clear_Window_State
     (flags : Interfaces.C.unsigned);
   pragma Import (C, Clear_Window_State, "ClearWindowState");

   procedure Toggle_Borderless_Windowed;
   pragma Import (C, Toggle_Borderless_Windowed, "ToggleBorderlessWindowed");

   procedure Set_Window_Icons
     (images : access Rayda_Types.Image;
      count : Interfaces.C.int);
   pragma Import (C, Set_Window_Icons, "SetWindowIcons");

   procedure Set_Window_Max_Size
     (width, height : Interfaces.C.int);
   pragma Import (C, Set_Window_Max_Size, "SetWindowMaxSize");

   procedure Set_Window_Opacity
     (opacity : Interfaces.C.C_float);
   pragma Import (C, Set_Window_Opacity, "SetWindowOpacity");

   procedure Set_Window_Focused;
   pragma Import (C, Set_Window_Focused, "SetWindowFocused");

   function Get_Monitor_Count return Interfaces.C.int;
   pragma Import (C, Get_Monitor_Count, "GetMonitorCount");

   function Get_Current_Monitor return Interfaces.C.int;
   pragma Import (C, Get_Current_Monitor, "GetCurrentMonitor");

   function Get_Monitor_Position
     (monitor : Interfaces.C.int) return Rayda_Types.Vector2;
   pragma Import (C, Get_Monitor_Position, "GetMonitorPosition");

   function Get_Monitor_Physical_Width
     (monitor : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Monitor_Physical_Width, "GetMonitorPhysicalWidth");

   function Get_Monitor_Physical_Height
     (monitor : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Monitor_Physical_Height, "GetMonitorPhysicalHeight");

   function Get_Monitor_Refresh_Rate
     (monitor : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Monitor_Refresh_Rate, "GetMonitorRefreshRate");

   function Get_Window_Scale_DPI return Rayda_Types.Vector2;
   pragma Import (C, Get_Window_Scale_DPI, "GetWindowScaleDPI");

   function Get_Monitor_Name
     (monitor : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Monitor_Name, "GetMonitorName");

   procedure Set_Clipboard_Text
     (text : Interfaces.C.char_array);
   pragma Import (C, Set_Clipboard_Text, "SetClipboardText");

   function Get_Clipboard_Image return Rayda_Types.Image;
   pragma Import (C, Get_Clipboard_Image, "GetClipboardImage");

   function Get_Clipboard_Text return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Clipboard_Text, "GetClipboardText");

   procedure Enable_Event_Waiting;
   pragma Import (C, Enable_Event_Waiting, "EnableEventWaiting");

   procedure Disable_Event_Waiting;
   pragma Import (C, Disable_Event_Waiting, "DisableEventWaiting");

end Rayda;
