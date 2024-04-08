unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics;

type
  // TForm1 is the main form class for the application.
  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject); // Called for drawing on the form.
  private
    // Procedure to draw a star at the center of the form.
    procedure DrawStar;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

// This procedure is called when the form needs repainting.
procedure TForm1.FormPaint(Sender: TObject);
begin
  DrawStar; // Draw the star at the center of the form.
end;

// Draws a five-pointed star at the center of the form.
procedure TForm1.DrawStar;
const
  StarSize = 50; // Size of the star.
  NumPoints = 5; // Number of points in the star. Thia value should be 5.
var
  Points: array[0..9] of TPoint; // Array to store the points of the star.
  i: Integer;
  Angle: Double;
begin
  // Calculate the points of the star.
  for i := 0 to NumPoints - 1 do
  begin
    Angle := -Pi / 2 + (i * 2 * Pi / NumPoints);
    Points[i * 2].X := Round(ClientWidth / 2 + StarSize * Cos(Angle));
    Points[i * 2].Y := Round(ClientHeight / 2 + StarSize * Sin(Angle));

    Angle := -Pi / 2 + ((i + 0.5) * 2 * Pi / NumPoints);
    Points[i * 2 + 1].X := Round(ClientWidth / 2 + StarSize * Cos(Angle) / 2);
    Points[i * 2 + 1].Y := Round(ClientHeight / 2 + StarSize * Sin(Angle) / 2);
  end;

  // Draw the star by connecting the points.
  form1.color := clblack;
  Canvas.Pen.Color := clyellow; // Set the color of the star.
  Canvas.Pen.Width := 4;     // Set the width of the star's lines.
  Canvas.Brush.Style := bsClear; // Set the brush style.

  Canvas.Polygon(Points); // Draw the star.
end;

end.
