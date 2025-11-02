with Interfaces.C;
with Raylib_Types;

package Raylib_Bindings is
   pragma Preelaborate;
   
   use type Raylib_Types.Color;
   use type Raylib_Types.Vector2;
   use type Raylib_Types.Vector3;
   use type Raylib_Types.Rectangle;
   use type Raylib_Types.Texture;
   use type Raylib_Types.Sound;
   use type Raylib_Types.Font;
   use type Raylib_Types.Camera2D;
   use type Raylib_Types.Camera3D;
   
   procedure Init_Window(width, height : Interfaces.C.int; title : Interfaces.C.char_array);
   pragma Import (C, Init_Window, "InitWindow");
   
   procedure Close_Window;
   pragma Import (C, Close_Window, "CloseWindow");
   
   function Window_Should_Close return Interfaces.C.int;
   pragma Import (C, Window_Should_Close, "WindowShouldClose");
   
   procedure Set_Target_FPS(fps : Interfaces.C.int);
   pragma Import (C, Set_Target_FPS, "SetTargetFPS");
   
   function Get_FPS return Interfaces.C.int;
   pragma Import (C, Get_FPS, "GetFPS");
   
   procedure Begin_Drawing;
   pragma Import (C, Begin_Drawing, "BeginDrawing");
   
   procedure End_Drawing;
   pragma Import (C, End_Drawing, "EndDrawing");
   
   procedure Clear_Background(color : Raylib_Types.Color);
   pragma Import (C, Clear_Background, "ClearBackground");
   
   procedure Draw_Circle(center_x, center_y : Interfaces.C.int; radius : Interfaces.C.C_float; color : Raylib_Types.Color);
   pragma Import (C, Draw_Circle, "DrawCircle");
   
   procedure Draw_Circle_V(center : Raylib_Types.Vector2; radius : Interfaces.C.C_float; color : Raylib_Types.Color);
   pragma Import (C, Draw_Circle_V, "DrawCircleV");
   
   procedure Draw_Rectangle(pos_x, pos_y, width, height : Interfaces.C.int; color : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle, "DrawRectangle");
   
   procedure Draw_Rectangle_V(position : Raylib_Types.Vector2; size : Raylib_Types.Vector2; color : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle_V, "DrawRectangleV");
   
   procedure Draw_Rectangle_Rec(rec : Raylib_Types.Rectangle; color : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle_Rec, "DrawRectangleRec");
   
   procedure Draw_Line(start_pos_x, start_pos_y, end_pos_x, end_pos_y : Interfaces.C.int; color : Raylib_Types.Color);
   pragma Import (C, Draw_Line, "DrawLine");
   
   procedure Draw_Triangle(v1, v2, v3 : Raylib_Types.Vector2; color : Raylib_Types.Color);
   pragma Import (C, Draw_Triangle, "DrawTriangle");
   
   function Load_Texture(file_name : Interfaces.C.char_array) return Raylib_Types.Texture;
   pragma Import (C, Load_Texture, "LoadTexture");
   
   procedure Unload_Texture(texture : Raylib_Types.Texture);
   pragma Import (C, Unload_Texture, "UnloadTexture");
   
   procedure Draw_Texture(texture : Raylib_Types.Texture; pos_x, pos_y : Interfaces.C.int; tint : Raylib_Types.Color);
   pragma Import (C, Draw_Texture, "DrawTexture");
   
   procedure Draw_Text(text : Interfaces.C.char_array; pos_x, pos_y, font_size : Interfaces.C.int; color : Raylib_Types.Color);
   pragma Import (C, Draw_Text, "DrawText");
   
   function Measure_Text(text : Interfaces.C.char_array; font_size : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Measure_Text, "MeasureText");
   
   function Is_Key_Pressed(key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Pressed, "IsKeyPressed");
   
   function Is_Key_Down(key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Down, "IsKeyDown");
   
   function Is_Key_Released(key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Released, "IsKeyReleased");

   function Is_Key_Up(key : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Key_Up, "IsKeyUp");

   function Get_Key_Pressed return Interfaces.C.int;
   pragma Import (C, Get_Key_Pressed, "GetKeyPressed");

   function Get_Char_Pressed return Interfaces.C.int;
   pragma Import (C, Get_Char_Pressed, "GetCharPressed");

   function Is_Mouse_Button_Pressed(button : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Is_Mouse_Button_Pressed, "IsMouseButtonPressed");
   
   function Get_Mouse_Position return Raylib_Types.Vector2;
   pragma Import (C, Get_Mouse_Position, "GetMousePosition");
   
   procedure Begin_Mode2D(camera : Raylib_Types.Camera2D);
   pragma Import (C, Begin_Mode2D, "BeginMode2D");
   
   procedure End_Mode2D;
   pragma Import (C, End_Mode2D, "EndMode2D");
   
   procedure Begin_Mode3D(camera : Raylib_Types.Camera3D);
   pragma Import (C, Begin_Mode3D, "BeginMode3D");
   
   procedure End_Mode3D;
   pragma Import (C, End_Mode3D, "EndMode3D");
   
   function Get_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Time, "GetTime");
   
   function Get_Frame_Time return Interfaces.C.C_float;
   pragma Import (C, Get_Frame_Time, "GetFrameTime");
   
   procedure Init_Audio_Device;
   pragma Import (C, Init_Audio_Device, "InitAudioDevice");
   
   procedure Close_Audio_Device;
   pragma Import (C, Close_Audio_Device, "CloseAudioDevice");
   
   function Load_Sound(file_name : Interfaces.C.char_array) return Raylib_Types.Sound;
   pragma Import (C, Load_Sound, "LoadSound");
   
   procedure Play_Sound(sound : Raylib_Types.Sound);
   pragma Import (C, Play_Sound, "PlaySound");
   
   procedure Stop_Sound(sound : Raylib_Types.Sound);
   pragma Import (C, Stop_Sound, "StopSound");

   procedure Pause_Sound(sound : Raylib_Types.Sound);
   pragma Import (C, Pause_Sound, "PauseSound");

   procedure Resume_Sound(sound : Raylib_Types.Sound);
   pragma Import (C, Resume_Sound, "ResumeSound");

   procedure Set_Sound_Volume(sound : Raylib_Types.Sound; volume : Interfaces.C.C_float);
   pragma Import (C, Set_Sound_Volume, "SetSoundVolume");

   procedure Set_Window_Title(title : Interfaces.C.char_array);
   pragma Import (C, Set_Window_Title, "SetWindowTitle");

   procedure Toggle_Fullscreen;
   pragma Import (C, Toggle_Fullscreen, "ToggleFullscreen");

   procedure Draw_Circle_Gradient(center_x, center_y : Interfaces.C.int; radius : Interfaces.C.C_float; color1, color2 : Raylib_Types.Color);
   pragma Import (C, Draw_Circle_Gradient, "DrawCircleGradient");

   procedure Draw_Rectangle_Gradient(pos_x, pos_y, width, height : Interfaces.C.int; color1, color2 : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle_Gradient, "DrawRectangleGradient");

   procedure Draw_Rectangle_Lines(pos_x, pos_y, width, height : Interfaces.C.int; color : Raylib_Types.Color);
   pragma Import (C, Draw_Rectangle_Lines, "DrawRectangleLines");

   procedure Draw_Text_Ex(font : Raylib_Types.Font; text : Interfaces.C.char_array; position : Raylib_Types.Vector2; font_size, spacing : Interfaces.C.C_float; tint : Raylib_Types.Color);
   pragma Import (C, Draw_Text_Ex, "DrawTextEx");

   function Measure_Text_Ex(font : Raylib_Types.Font; text : Interfaces.C.char_array; font_size, spacing : Interfaces.C.C_float) return Raylib_Types.Vector2;
   pragma Import (C, Measure_Text_Ex, "MeasureTextEx");
end Raylib_Bindings;
