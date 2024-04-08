/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////
/////  Important Notice:
/////
/////  Do not remove this comment when using this program.
/////  Removing it will result in a copyright infringement.
/////  As long as this comment remains, the author, MoonWolf,
/////  allows you to freely utilize this program.
/////
/////  Usage examples: Personal program study, showing to
/////  friends, presentations at school,
/////  submission as a free assignment during winter or
/////  summer holidays, reference material for graduation thesis,
/////  teachers demonstrating functions to students using this program.
/////  You can use it freely without MoonWolf's consent.
/////
/////  The program can be partly modified to
/////  customize the graph shape and more.
/////  Particularly, fine-tuning is possible through
/////  constant definitions in the Const section.
/////
/////  All rights reserved
/////  (c)2024 MoonWolf
/////
/////  Program: Simple 2D Game with Math function assisted by ChatGPT4
/////  Version: 1.00
/////  Created by: MoonWolf / Assisted by ChatGPT4
/////  Twitter: MoonWolf_001
/////  Development Environment: Window11 + Lazarus 2.2.6
/////  Release Date: January 26, 2024
/////
/////  Remarks:
/////
/////  Lazarus is a programming development environment.
/////  It's a free programming development environment.
/////  The programming language is Pascal.
/////  For programming beginners, it's less difficult than
/////  C language-based ones and easy to get started.
/////  Pascal is used as an educational programming language in the world.
/////  Lazarus is simple and powerful.
/////  Almost all Windows apps can be developed using Lazarus.
/////
/////  Lazarus Official Website:
/////  https://lazarus-ide.org
/////
/////  Reference Book:
/////
/////  SIMPLE 2D GAME: PASCAL Programming
/////
/////  Author MoonWolf JP. Published in 2023
/////  Available as Kindle e-books and paperback.
/////  Also available for unlimited reading for Kindle Unlimited subscribers.
/////  Amazon Link:
/////  https://www.amazon.com/dp/B0CHN7PK88
/////
/////  Disclaimer:
/////  MoonWolf does not provide support for Windows knowledge,
/////  Lazarus installation,
/////  questions about Lazarus, errors that occur,
/////  and any other issues or questions related to these.
/////  Please research and solve these issues on your own.
/////  Using the free version of ChatPGT3.5
/////  can potentially help solve these efficiently.
/////  Using the paid version of ChatGPT4
/////  can potentially help solve these more efficiently.
/////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, Math, LCLType, Dialogs;

type
  // TForm1 is the main form class for the application.
  // It handles all the visual elements and game logic.
  TForm1 = class(TForm)
    Timer1: TTimer; // Timer for continuous updates during gameplay.
    procedure FormCreate(Sender: TObject); // Called when the form is created.
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject); // Called for drawing on the form.
    procedure Timer1Timer(Sender: TObject); // Called on each timer tick.
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); // Called when a mouse button is clicked.
  private
    ShowHitMessage: Boolean; // Whether to display the hit message
    HitMessageTimer: Integer; // Timer to manage the display duration of the hit message
    FunctionRepeatCount: Integer; // Variable to track the number of times a function is repeated
    GameOver: Boolean; // Tracks the game over state

    // Variables to track the star's position and current mathematical function.
    StarX, StarY: Integer;
    CurrentFunctionIndex: Integer;

    // Variables to manage the bullets' state and position.
    BulletActiveLeft, BulletActiveRight: Boolean;
    BulletXLeft, BulletYLeft: Integer;
    BulletXRight, BulletYRight: Integer;

    // Variables for scoring and target positioning.
    TargetX, TargetY: Integer;
    Score: Integer;
    ShowTargetMark: Boolean;

    // Additional variables for dynamic bullet behavior.
    BulletRadiusLeft, BulletRadiusRight: Integer; // Radius of the bullets.
    DeltaXLeft, DeltaYLeft: Integer; // Movement per frame for left bullet.
    DeltaXRight, DeltaYRight: Integer; // Movement per frame for right bullet.
    DeltaRadiusLeft, DeltaRadiusRight: Integer; // Radius reduction per frame.

    InitialMessageFontSize: Integer; // Initial font size of the message
    MaxHitMessageTimer: Integer; // Maximum display time of the message
    FirstClickOccurred: Boolean; // Variable to track whether the first mouse click has occurred

    // Procedures for drawing various elements on the form.
    procedure DisplayGameOverMessage;
    procedure DrawStar(const CenterX, CenterY, Size: Integer;
                       const LineColor: TColor; const LineWidth: Integer);
    procedure DrawAxes;
    procedure DrawGrid;
    procedure DrawFunctionText;
    procedure DrawStarCoordinates(X, Y: Integer);
    procedure DrawBullet(const X, Y, Radius: Integer);
    procedure DrawScore;
    procedure DrawTargetMark(const X, Y: Integer);

    // Function to calculate the Y-coordinate based on a mathematical function.
    function CalculateY(X: Integer): Integer;
  public
  end;

var
  Form1: TForm1;

const
  // Constants for graphical elements and game mechanics.
  StarSize = 20; // Size of the star.
  StarColor = clYellow; // Color of the star.
  StarLineWidth = 7; // Width of the star's outline.
  GraphScale = 100; // Scaling factor for graphing functions.

  // Array of mathematical functions represented in the game.
  GraphFunctions: array[0..4] of Integer = (0, 1, 2, 3, 4);
  FunctionTexts: array[0..4] of string = (
    'Y = 0.0077 * X^2 - 150', // Quadratic function.
    'Y = |0.5 * X + 100|', // Absolute value function.
    'Y = 150 * Sin(0.03 * X)', // Sine function.
    'Y = 9000 / X (X ≠ 0)', // Inverse function.
    'Y = 10 * tan(0.0099 * X) (cos(0.0099X) ≠ 0)' // Tangent function.
  );

  // Additional constants for UI elements.
  FunctionTextFontColor = clWhite; // Font color for displaying function text.
  FunctionTextFontSize = 25; // Font size for displaying function text.
  FunctionTextPositionX = 10; // X-coordinate for function text position.
  FunctionTextPositionY = 400; // Y-coordinate for function text position.
  AxisLabelFontColor = clWhite; // Font color for axis labels.
  AxisLabelFontSize = 8; // Font size for axis labels.
  ArrowSize = 10; // Size of the axis arrows.
  GridColor = clGray; // Color of the grid lines.
  GridSpacing = 50; // Spacing between grid lines.
  GridLabelFontSize = 8; // Font size for grid labels.
  TimerInterval = 40; // Interval for the game timer.
  StarMoveStep = 5; // Movement step size for the star.
  StarCoordFontColor = clWhite; // Font color for star coordinates.
  StarCoordFontSize = 25; // Font size for star coordinates.
  StarCoordPositionX = 10; // X-coordinate for star coordinates position.
  StarCoordPositionY = 450; // Y-coordinate for star coordinates position.
  HitMessageDuration = 14; // Number of frames to display the hit message (0.7 seconds).
  InitialBulletRadius = 50; // Initial radius of the bullets.
  FinalBulletRadius = 10; // Final radius of the bullets.
  BulletShrinkRate = 5; // Rate at which bullets shrink.
  BulletSpeed = 5; // Speed of the bullets.
  HitTolerance = 15; // Tolerance for hit detection.
  ScoreHit = 100; // Points awarded for a hit.
  ScoreMiss = -10; // Points deducted for a miss.
  BulletColor = clRed; // Color of the bullets.
  BulletBorderWidth = 5; // Width of the bullet outline.
  TargetHitTolerance = 15; // Tolerance for hitting the target.
  TargetMarkColor = clLime; // Color of the target marker.
  TargetMarkSize = 10; // Size of the target marker.
  TargetMarkAdjustX = 0; // X-coordinate adjustment for target marker.
  TargetMarkAdjustY = 0; // Y-coordinate adjustment for target marker.
  TotalFrames = 20; // Number of frames for bullet movement.

implementation

{$R *.lfm}

{ TForm1 }

// Initializes game settings and positions the star when the form is created.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Simple 2D Game with Math function with GPT4 by MoonWolf - Jan 26, 2024, Version 1.00';
  StarX := -ClientWidth div 2;
  CurrentFunctionIndex := 0;
  Timer1.Interval := TimerInterval;
  Timer1.Enabled := True;
  BulletActiveLeft := False;
  BulletActiveRight := False;
  Score := 0;
  ShowHitMessage := False;
  HitMessageTimer := 0;

  InitialMessageFontSize := 120; // Set initial font size for hit message
  MaxHitMessageTimer := 15;      // Set maximum display time for hit message
  FirstClickOccurred := False;   // Tracks if the first mouse click has occurred
  FunctionRepeatCount := 0;
  GameOver := False;
end;

// Redraws the form including the grid, axes, functions, star, bullets, and target mark.
procedure TForm1.FormPaint(Sender: TObject);
var
  CurrentFontSize: Integer;
  TextPosX, TextPosY: Integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);
  DrawGrid;
  DrawAxes;
  DrawFunctionText;

  StarY := CalculateY(StarX) + (ClientHeight div 2);
  DrawStar(StarX + (ClientWidth div 2), StarY, StarSize, StarColor, StarLineWidth);
  DrawStarCoordinates(StarX, StarY);

  if BulletActiveLeft then
    DrawBullet(BulletXLeft, BulletYLeft, BulletRadiusLeft);
  if BulletActiveRight then
    DrawBullet(BulletXRight, BulletYRight, BulletRadiusRight);
  if ShowTargetMark then
    DrawTargetMark(TargetX + TargetMarkAdjustX, TargetY + TargetMarkAdjustY);
  DrawScore;

  if ShowHitMessage then
  begin
    CurrentFontSize := Max(12, InitialMessageFontSize * HitMessageTimer div MaxHitMessageTimer);
    Canvas.Font.Size := CurrentFontSize;
    Canvas.Font.Color := clWhite;
    Canvas.TextOut(StarX + (ClientWidth div 2) - CurrentFontSize * 2, StarY + 20, 'Nice Hit!');
  end;
end;

// Handles star and bullet movement, score updates, and game over condition on each timer tick.
procedure TForm1.Timer1Timer(Sender: TObject);
var
  TextPosX, TextPosY: Integer;
begin
  // Independent movement of the star
  Inc(StarX, StarMoveStep);
  if StarX > ClientWidth div 2 then
  begin
    StarX := -ClientWidth div 2;
    Inc(CurrentFunctionIndex);
    if CurrentFunctionIndex >= Length(GraphFunctions) then
    begin
      CurrentFunctionIndex := 0;
      Inc(FunctionRepeatCount);
      if FunctionRepeatCount >= 2 then // Set condition for Game Over
      begin
        GameOver := True;
        DisplayGameOverMessage; // Call new procedure to display Game Over message
        Timer1.Enabled := False; // Stop the timer
        Exit; // Do not execute further code
      end;
    end;
  end
  else if StarX < -ClientWidth div 2 then
  begin
    StarX := ClientWidth div 2;
    CurrentFunctionIndex := (CurrentFunctionIndex + 1) mod Length(GraphFunctions);
  end;

  // Adjusting the movement of the left bullet
  if BulletActiveLeft then
  begin
    BulletXLeft := BulletXLeft + DeltaXLeft;
    BulletYLeft := BulletYLeft + DeltaYLeft;
    BulletRadiusLeft := Max(BulletRadiusLeft - DeltaRadiusLeft, 10);

    if (Abs(BulletXLeft - TargetX) <= TargetHitTolerance) and
       (Abs(BulletYLeft - TargetY) <= TargetHitTolerance) then
    begin
      BulletActiveLeft := False;
      BulletXLeft := TargetX;
      BulletYLeft := TargetY;
      BulletRadiusLeft := 10;
      if not BulletActiveRight then
        ShowTargetMark := False;
    end;
  end;

  // Adjusting the movement of the right bullet
  if BulletActiveRight then
  begin
    BulletXRight := BulletXRight + DeltaXRight;
    BulletYRight := BulletYRight + DeltaYRight;
    BulletRadiusRight := Max(BulletRadiusRight - DeltaRadiusRight, 10);

    if (Abs(BulletXRight - TargetX) <= TargetHitTolerance) and
       (Abs(BulletYRight - TargetY) <= TargetHitTolerance) then
    begin
      BulletActiveRight := False;
      BulletXRight := TargetX;
      BulletYRight := TargetY;
      BulletRadiusRight := 10;
      if not BulletActiveLeft then
        ShowTargetMark := False;
    end;
  end;

  // Updating the score once the bullets have finished moving
  if (not BulletActiveLeft) and (not BulletActiveRight) and FirstClickOccurred then
  begin
    // Hit detection
    if (Abs(StarX + (ClientWidth div 2) - TargetX) <= HitTolerance) and
       (Abs(StarY - TargetY) <= HitTolerance) then
    begin
      Score := Score + ScoreHit;
      ShowHitMessage := True; // Display hit message
      HitMessageTimer := HitMessageDuration; // Set message display duration
    end
    else
      Score := Score + ScoreMiss;

    // Reactivate bullets to prevent reactivation
    BulletActiveLeft := True;
    BulletActiveRight := True;
  end;

  if ShowHitMessage then
  begin
    Dec(HitMessageTimer);
    if HitMessageTimer <= 0 then
    begin
      ShowHitMessage := False;
    end;
  end;
  Invalidate; // Redrawing the screen
end;

// Initializes bullets and sets their trajectory when the mouse button is pressed.
procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  InitialBulletRadius = 100; // Initial radius of the bullets.
  FinalBulletRadius = 10;    // Final radius of the bullets.
  TotalFrames = 10;      // Number of frames for bullet movement.
begin
  FirstClickOccurred := True; // Flag set to True when the mouse is clicked

  TargetX := X; // Set the target X-coordinate to the clicked position.
  TargetY := Y; // Set the target Y-coordinate to the clicked position.

  // Initialize the left bullet at the left edge of the form.
  BulletActiveLeft := True;
  BulletXLeft := 0; // Start from the left edge.
  BulletYLeft := Form1.Height div 2; // Start from the vertical center.
  BulletRadiusLeft := InitialBulletRadius; // Set the initial radius.
  DeltaRadiusLeft := (InitialBulletRadius - FinalBulletRadius) div TotalFrames; // Set radius decrease per frame.
  DeltaXLeft := (TargetX - BulletXLeft) div TotalFrames; // Set horizontal movement per frame.
  DeltaYLeft := (TargetY - BulletYLeft) div TotalFrames; // Set vertical movement per frame.

  // Initialize the right bullet at the right edge of the form.
  BulletActiveRight := True;
  BulletXRight := Form1.Width; // Start from the right edge.
  BulletYRight := Form1.Height div 2; // Start from the vertical center.
  BulletRadiusRight := InitialBulletRadius; // Set the initial radius.
  DeltaRadiusRight := (InitialBulletRadius - FinalBulletRadius) div TotalFrames; // Set radius decrease per frame.
  DeltaXRight := (TargetX - BulletXRight) div TotalFrames; // Set horizontal movement per frame.
  DeltaYRight := (TargetY - BulletYRight) div TotalFrames; // Set vertical movement per frame.

  ShowTargetMark := True; // Show the target mark at the clicked position.
end;

// Draws a star at the given position with specified size, color, and line width.
procedure TForm1.DrawStar(const CenterX, CenterY, Size: Integer;
                          const LineColor: TColor; const LineWidth: Integer);
var
  Points: array[0..9] of TPoint; // Array to store the points of the star.
  i: Integer;
  Angle: Double;
begin
  // Calculate the points of the star.
  for i := 0 to 4 do
  begin
    Angle := -Pi / 2 + (i * 2 * Pi / 5);
    Points[i * 2].X := CenterX + Round(Size * Cos(Angle));
    Points[i * 2].Y := CenterY + Round(Size * Sin(Angle));

    Angle := -Pi / 2 + ((i + 0.5) * 2 * Pi / 5);
    Points[i * 2 + 1].X := CenterX + Round(Size * Cos(Angle) / 2);
    Points[i * 2 + 1].Y := CenterY + Round(Size * Sin(Angle) / 2);
  end;

  // Draw the star by connecting the points.
  Canvas.Pen.Color := LineColor;
  Canvas.Pen.Width := LineWidth;
  Canvas.Brush.Style := bsClear;

  Canvas.MoveTo(Points[0].X, Points[0].Y);
  for i := 1 to 10 do
  begin
    Canvas.LineTo(Points[i mod 10].X, Points[i mod 10].Y);
  end;
end;

// Draws coordinate axes on the form.
procedure TForm1.DrawAxes;
begin
  // Drawing the X and Y axes in white.
  Canvas.Pen.Color := clWhite;
  Canvas.Pen.Width := 2;
  Canvas.MoveTo(0, ClientHeight div 2);
  Canvas.LineTo(ClientWidth, ClientHeight div 2);
  Canvas.MoveTo(ClientWidth div 2, 0);
  Canvas.LineTo(ClientWidth div 2, ClientHeight);

  // Labeling the axes.
  Canvas.Font.Color := AxisLabelFontColor;
  Canvas.Font.Size := AxisLabelFontSize;
  Canvas.TextOut(ClientWidth - 20, ClientHeight div 2 + 5, 'X');
  Canvas.TextOut(ClientWidth div 2 + 5, 5, 'Y');
  Canvas.TextOut(ClientWidth div 2 + 5, ClientHeight div 2 + 5, 'O');
end;

// Draws the grid on the form.
procedure TForm1.DrawGrid;
var
  i: Integer;
  GridX, GridY: Integer;
begin
  // Setting the grid color and line width.
  Canvas.Pen.Color := GridColor;
  Canvas.Pen.Width := 1;
  Canvas.Font.Size := GridLabelFontSize;

  // Drawing grid lines along the X-axis and labeling them.
  for i := -ClientWidth div 2 to ClientWidth div 2 do
  begin
    if (i mod GridSpacing = 0) and (i <> 0) then
    begin
      GridX := i + (ClientWidth div 2);
      Canvas.MoveTo(GridX, 0);
      Canvas.LineTo(GridX, ClientHeight);
      Canvas.TextOut(GridX - 10, ClientHeight div 2 + 5, IntToStr(i));
    end;
  end;

  // Drawing grid lines along the Y-axis and labeling them.
  for i := -ClientHeight div 2 to ClientHeight div 2 do
  begin
    if (i mod GridSpacing = 0) and (i <> 0) then
    begin
      GridY := i + (ClientHeight div 2);
      Canvas.MoveTo(0, GridY);
      Canvas.LineTo(ClientWidth, GridY);
      Canvas.TextOut(ClientWidth div 2 + 5, GridY - 10, IntToStr(-i));
    end;
  end;
end;

// Displays the current mathematical function as text on the form.
procedure TForm1.DrawFunctionText;
begin
  // Setting the font color and size for the function text.
  Canvas.Font.Color := FunctionTextFontColor;
  Canvas.Font.Size := FunctionTextFontSize;

  // Displaying the function text based on the current function index.
  Canvas.TextOut(FunctionTextPositionX, FunctionTextPositionY, FunctionTexts[CurrentFunctionIndex]);
end;

// Displays the coordinates of the star on the form.
procedure TForm1.DrawStarCoordinates(X, Y: Integer);
var
  CoordText: String;
begin
  // Formatting and displaying the star's coordinates.
  Y := -Y + (ClientHeight div 2); // Convert to Math Coordinate Plane
  Canvas.Font.Color := StarCoordFontColor;
  Canvas.Font.Size := StarCoordFontSize;
  CoordText := Format('(X=%d, Y=%d)', [X, Y]);
  Canvas.TextOut(StarCoordPositionX, StarCoordPositionY, CoordText);
end;

// Draws a bullet at the specified position with the given radius.
procedure TForm1.DrawBullet(const X, Y, Radius: Integer);
begin
  // Setting the color and border width for the bullet.
  Canvas.Pen.Color := BulletColor;
  Canvas.Pen.Width := BulletBorderWidth;
  Canvas.Brush.Style := bsClear;

  // Drawing the bullet as an ellipse.
  Canvas.Ellipse(X - Radius, Y - Radius, X + Radius, Y + Radius);
end;

// Displays the current score on the form.
procedure TForm1.DrawScore;
begin
  // Setting the font color and size for the score.
  Canvas.Font.Color := clWhite;
  Canvas.Font.Size := 20;

  // Displaying the score text.
  Canvas.TextOut(10, 10, 'Score: ' + IntToStr(Score));
end;

// Draws a target mark at the specified position.
procedure TForm1.DrawTargetMark(const X, Y: Integer);
begin
  // Setting the color for the target mark.
  Canvas.Pen.Color := TargetMarkColor;

  // Drawing horizontal and vertical lines to represent the target mark.
  Canvas.MoveTo(X - TargetMarkSize, Y);
  Canvas.LineTo(X + TargetMarkSize, Y);
  Canvas.MoveTo(X, Y - TargetMarkSize);
  Canvas.LineTo(X, Y + TargetMarkSize);
end;

// Calculates the Y-coordinate for the current function based on the given X-coordinate.
function TForm1.CalculateY(X: Integer): Integer;
begin

// Calculating the Y-coordinate based on the selected function.

  case GraphFunctions[CurrentFunctionIndex] of

      0: Result := Round(0.0077 * Power(X, 2)-150);
      1: Result := Round(Abs(0.5 * X +100 ));
      2: Result := Round(150 * Sin(0.03 * X));
      3: if X <> 0 then
           //Result := Round(7000 / X)
           Result := Round(9000 / X)
         else
           Result := 0;
      4: if Cos(0.0099 * X) <> 0 then
           Result := Round(10*Sin(0.0099 * X) / Cos(0.0099 * X))
         else
           Result := 0;
    else
      Result := 0;
    end;

    Result := - Result; // From1's Y direction is downward


end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Reset game if space key is pressed after Game Over
  if (Key = VK_SPACE) and GameOver then
  begin
    Score := 0;
    FunctionRepeatCount := 0;
    GameOver := False;
    Timer1.Enabled := True;
  end;

  // Shortcut for moving the star to a specific position
  // This is hidden command to reduce star moving time
  if (Key = VK_Right) then
  begin
    StarX := 300;
  end;

end;

{
procedure TForm1.ShowGameOverMessage; // This is only for debug
begin
  ShowMessage('Game Over');
end;
}

// Displays the game over message.
procedure TForm1.DisplayGameOverMessage;
begin
  // Setting font properties for Game Over message
  Canvas.Font.Size := 30;
  Canvas.Font.Color := clyellow;

  // Displaying Game Over message and score, centered on the screen
  Canvas.TextOut((ClientWidth - Canvas.TextWidth('Game Over')) div 2, (ClientHeight div 2) - 50, 'Game Over');
  Canvas.TextOut((ClientWidth - Canvas.TextWidth('Your Score = ' + IntToStr(Score))) div 2, ClientHeight div 2, 'Your Score = ' + IntToStr(Score));
  Canvas.TextOut((ClientWidth - Canvas.TextWidth('Hit Space Key to replay')) div 2, (ClientHeight div 2) + 50, 'Hit Space Key to replay');
end;

end.
