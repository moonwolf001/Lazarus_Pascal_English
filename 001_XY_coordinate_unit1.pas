unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, Math;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure DrawAxes;
    procedure DrawGrid;
    procedure DrawAxisNumbers;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

// Initializes the form.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'XY Coordinate Plane';
end;

// Handles the painting of the form.
procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clBlack; // Set background color to black
  Canvas.FillRect(ClientRect);   // Fill the form with the background color
  DrawGrid;                      // Draw the grid
  DrawAxes;                      // Draw the axes
  DrawAxisNumbers;               // Draw numbers on the axes
end;

// Draws the X and Y axes with labels and arrows.
procedure TForm1.DrawAxes;
const
  ArrowSize = 10; // Size of the arrows at the end of the axes
begin
  Canvas.Pen.Color := clWhite; // Set pen color to white for axes
  Canvas.Pen.Width := 2;       // Set pen width for axes
  Canvas.Font.Color := clWhite; // Set font color to white for labels

  // Draw X-axis
  Canvas.MoveTo(0, ClientHeight div 2);
  Canvas.LineTo(ClientWidth, ClientHeight div 2);
  // Arrow for X-axis
  Canvas.LineTo(ClientWidth - ArrowSize, (ClientHeight div 2) - ArrowSize);
  Canvas.MoveTo(ClientWidth, ClientHeight div 2);
  Canvas.LineTo(ClientWidth - ArrowSize, (ClientHeight div 2) + ArrowSize);

  // Draw Y-axis
  Canvas.MoveTo(ClientWidth div 2, ClientHeight);
  Canvas.LineTo(ClientWidth div 2, 0);
  // Arrow for Y-axis
  Canvas.LineTo((ClientWidth div 2) - ArrowSize, ArrowSize);
  Canvas.MoveTo(ClientWidth div 2, 0);
  Canvas.LineTo((ClientWidth div 2) + ArrowSize, ArrowSize);

  // Labels for axes
  Canvas.TextOut(ClientWidth - 20, ClientHeight div 2 + 5, 'X');
  Canvas.TextOut(ClientWidth div 2 + 5, 10, 'Y'); // Adjusted position for 'Y' label
  Canvas.TextOut(ClientWidth div 2 + 5, ClientHeight div 2 + 5, 'O');
end;

// Draws the grid on the form.
procedure TForm1.DrawGrid;
var
  i: Integer;
  GridSpacing: Integer = 50; // Spacing for the grid lines
begin
  Canvas.Pen.Color := clGray; // Set pen color for grid lines
  Canvas.Pen.Width := 1;      // Set pen width for grid lines

  // Draw grid lines
  for i := -ClientWidth div 2 to ClientWidth div 2 do
  begin
    if (i mod GridSpacing = 0) and (i <> 0) then
    begin
      Canvas.MoveTo(i + (ClientWidth div 2), 0);
      Canvas.LineTo(i + (ClientWidth div 2), ClientHeight);
      Canvas.MoveTo(0, i + (ClientHeight div 2));
      Canvas.LineTo(ClientWidth, i + (ClientHeight div 2));
    end;
  end;
end;

// Draws numbers on the axes at every 50 units.
procedure TForm1.DrawAxisNumbers;
var
  i: Integer;
  GridSpacing: Integer = 50; // Spacing for the numbers on the grid lines
begin
  Canvas.Font.Color := clWhite; // Set font color to white for numbers

  // Draw numbers along X-axis
  for i := -ClientWidth div 2 to ClientWidth div 2 do
  begin
    if (i mod GridSpacing = 0) and (i <> 0) then
    begin
      Canvas.TextOut(i + (ClientWidth div 2) - 10, (ClientHeight div 2) + 5, IntToStr(i));
    end;
  end;

  // Draw numbers along Y-axis
  for i := -ClientHeight div 2 to ClientHeight div 2 do
  begin
    if (i mod GridSpacing = 0) and (i <> 0) then
    begin
      Canvas.TextOut((ClientWidth div 2) + 5, i + (ClientHeight div 2) - 10, IntToStr(-i));
    end;
  end;
end;

end.
