unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer; // Timer for continuous updates during gameplay.
    procedure FormCreate(Sender: TObject); // Called when the form is created.
    procedure FormPaint(Sender: TObject); // Called for drawing on the form.
    procedure Timer1Timer(Sender: TObject); // Called on each timer tick.
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); // Called when a mouse button is clicked.
  private

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

    procedure DrawAxes;
    procedure DrawGrid;
    procedure DrawBullet(const X, Y, Radius: Integer);
    procedure DrawTargetMark(const X, Y: Integer);

  public
  end;

var
  Form1: TForm1;

const

  // Additional constants for UI elements.

  AxisLabelFontColor = clWhite;
  AxisLabelFontSize = 8;
  ArrowSize = 10;
  GridColor = clGray;
  GridSpacing = 50;
  GridLabelFontSize = 8;

  TimerInterval = 40;

  // Constants for bullet dynamics and game scoring.
  InitialBulletRadius = 50; // Initial radius of the bullets.
  FinalBulletRadius = 10;   // Final radius of the bullets.
  BulletShrinkRate = 5;     // Rate at which bullets shrink.
  BulletSpeed = 5;          // Speed of the bullets.
  HitTolerance = 10;        // Tolerance for hit detection.
  ScoreHit = 100;           // Points awarded for a hit.
  ScoreMiss = -10;          // Points deducted for a miss.
  BulletColor = clRed;      // Color of the bullets.
  BulletBorderWidth = 5;    // Width of the bullet outline.
  TargetHitTolerance = 15;  // Tolerance for hitting the target.
  TargetMarkColor = clLime; // Color of the target marker.
  TargetMarkSize = 10;      // Size of the target marker.
  TargetMarkAdjustX = 0;    // X-coordinate adjustment for target marker.
  TargetMarkAdjustY = 0;    // Y-coordinate adjustment for target marker.
  TotalFrames = 20;         // Number of frames for bullet movement.


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Bullet moving effect using Lazarus (PASCAL) by MoonWolf - Dec 27, 2023, Version 1.00';
  Timer1.Interval := TimerInterval;
  Timer1.Enabled := True;
  BulletActiveLeft := False;
  BulletActiveRight := False;
  Score := 0;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);
  DrawGrid;
  DrawAxes;

  if BulletActiveLeft then
    DrawBullet(BulletXLeft, BulletYLeft, BulletRadiusLeft);
  if BulletActiveRight then
    DrawBullet(BulletXRight, BulletYRight, BulletRadiusRight);
  if ShowTargetMark then
    DrawTargetMark(TargetX + TargetMarkAdjustX, TargetY + TargetMarkAdjustY);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin

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

  Invalidate; // Redrawing the screen
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  InitialBulletRadius = 100; // Initial radius of the bullets.
  FinalBulletRadius = 10;    // Final radius of the bullets.
  TotalFrames = 10;          // Number of frames for bullet movement.
begin
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

end.
