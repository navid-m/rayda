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
     (model         : Rayda_Types.Model;
      position      : Rayda_Types.Vector3;
      rotation_axis : Rayda_Types.Vector3;
      rotation_angle : Interfaces.C.C_float;
      scale         : Rayda_Types.Vector3;
      tint          : Rayda_Types.Color);
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
     (box   : Rayda_Types.Bounding_Box;
      color : Rayda_Types.Color);
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
     (camera    : Rayda_Types.Camera3D;
      texture   : Rayda_Types.Texture2D;
      source    : Rayda_Types.Rectangle;
      position  : Rayda_Types.Vector3;
      up        : Rayda_Types.Vector3;
      size      : Rayda_Types.Vector2;
      origin    : Rayda_Types.Vector2;
      rotation  : Interfaces.C.C_float;
      tint      : Rayda_Types.Color);
   pragma Import (C, Draw_Billboard_Pro, "DrawBillboardPro");

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
end Rayda;
