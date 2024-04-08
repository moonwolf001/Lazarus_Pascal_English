unit Unit1;

// programmed by MoonWolf in August 2023
// book: Simple 2D Game Pascal Programming
// Amazon link: https://www.amazon.com/dp/B0CHN7PK88
// Program language = PASCAL , Develop Environment = LAZARUS 2.2.2 Windows11
// Related Novel : AD 22XX Earth Defense Force Third Secret Unit
// Amazon link : https://www.amazon.com/dp/B0CG69B5YC
// Kindle and Paperback available , Author MoonWolf JP
// In the novel, this is not only a game but also this is imitation of real military training

{$mode objfpc}{$H+}

interface

uses
  // LCLType  :  added manually for VF(Virtual Function) Key
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,LCLType;

type

  TATE_bar_record = record     // Record type definition of Electro Magnetic Wave Device

    visible_flag : integer;   // 0 = NOT display , 1 = display
    X            : integer;   // X coordinate of top left of vertical square
    Y            : integer;   // Y coordinate of top left of vertical square
    x_bar_star   : integer;   // Difference in X coordinate between vertical square and star when clicked
    y_bar_star   : integer;   // Difference in  coordinate between vertical square and star when clicked
    x_delta      : integer;   // At Timer2, this delta value is closer to the star (X coordinate)
    y_delta      : integer;   // At Timer2, this delta value is closer to the star (Y coordinate)
    count        : integer;   // Use Timer2 to move closer to the star for this count number of times.
    height        : integer;   // Vertical square image height
    width        : integer;   // Vertical square image width

    end;

  Aim_Score_record = record

    X            : integer;  // X coordinate used for hit detection
    Y            : integer;  // Y coordinate used for hit detection
    count        : integer;  // Count of “Nice!!” display after hit determination
    Score        : integer;

  end;

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

var
  X,Y                : integer; // "Pure XY coordinates" handled by Timer1 and Timer2
  GM_Level           : integer; // Game Level
  Fx_name            : string ; // The currently drawn y=f(x) is expressed as a string
  Tate_Bar           : array[1..3] of tate_bar_record ;  // Super ElectoroMag Wave
  x_mouse, y_mouse   : integer; // X,Y coordinates of pointer on Form1 when mouse clicks
  AIM_SCORE          : array[1..2] of Aim_Score_record ;

const
  x_cnt          = 363;          // X coordinate of origin of image1 coordinate plane on Form1
  y_cnt          = 190;          // Y coordinate of origin of image1 coordinate plane on Form1
  bar_star_step  = 20;           // Number of times: Move from vertical square to star

  version        = 'Ver 2.00';   // 2023.07,20 : Version 1.00 Release of original Japanese version
                                 // 2023.09.04 : Version 2.00 Release of English version

{$R *.lfm}

{ TForm1 }

Procedure GAME_OVER_COMMENT ;  // Comment processing during GAME OVER
var
  Item1    : array[1..10] of string;
  Item2    : array[1..10] of string;
  Mary     : array[1..10] of string;
  MoonWolf : array[1..10] of string;

  item1_name        : string;
  item2_name        : string;
  Mary_comment      : string;
  MoonWolf_Comment  : string;

begin
  // Item 1
  item1[1]  := 'Mystery Stone';
  item1[2]  := 'Extraterrestrial Gem';
  item1[3]  := 'Extraterrestrial Fossil';
  item1[4]  := 'An ore that can extract unlimited energy';
  item1[5]  := '10 grams of rare Ore';
  item1[6]  := 'Ore that distorts space';
  item1[7]  := 'Faintly warm Ore';
  item1[8]  := 'Ore that can be spotted by aliens';
  item1[9]  := 'Ore that smells good';
  item1[10] := 'Good sounding Ore';

  // Item 2
  item2[1]  := '5 minutes transparent ticket (2 tickets)';
  item2[2]  := '10x Courage Ticket (12 tickets)';
  item2[3]  := 'Newly discovered tickets (37 tickets)';
  item2[4]  := 'Mathematics skill up ticket (37 tickets)';
  item2[5]  := 'Space trip for 3 days and 2 nights (37 tickets)';
  item2[6]  := '10 minutes future society experience (2 tickets)';
  item2[7]  := 'Conversation tickets with animals (4 tickets)';
  item2[8]  := 'Aerial walk tickets (5 tickets)';
  item2[9]  := 'Program power up (10 tickets)';
  item2[10] := 'Teleportation (3 tickets)';

  // comment from C225 Mary
  Mary[1]   := 'You must be tired';
  Mary[2]   := 'How about a cup of coffee';
  Mary[3]   := 'Oh, you did well';
  Mary[4]   := 'mission completed!';
  Mary[5]   := 'You can do more';
  Mary[6]   := 'Have a nice day';
  Mary[7]   := 'remember me sometimes';
  Mary[8]   := 'Good Job';
  Mary[9]   := 'you still don not have enough practice';
  Mary[10]  := 'Humanity may be wiped out...';

  // comment from Captain MoonWolf
  MoonWolf[1]  := 'Good Job!';
  MoonWolf[2]  := 'Nice Challenge';
  MoonWolf[3]  := 'there is something to see';
  MoonWolf[4]  := 'I was waiting for someone like you';
  MoonWolf[5]  := 'Wonderful';
  MoonWolf[6]  := 'Would you like to join my squad?';
  MoonWolf[7]  := 'That is a great effort';
  MoonWolf[8]  := 'A good future seems to open up';
  MoonWolf[9]  := 'You are a very valuable person.';
  MoonWolf[10] := 'That is great concentration';

  randomize;        // random number initialization

  item1_name        := item1[     random(10)+1   ] ;   // item1 selected at random
  item2_name        := item2[     random(10)+1   ] ;   // item2 selected at random
  Mary_comment      := Mary[      random(10)+1   ] ;   // C225 comment at random
  MoonWolf_Comment  := MoonWolf[  random(10)+1   ] ;   // MoonWolf comment at random

  // Comments to users on MEMO 1 at the end of the game

  form1.Memo1.Append( '=====================');
  form1.Memo1.Append( 'Your SCORE = ' + IntToStr(AIM_SCORE[1].Score) );
  form1.Memo1.Append( '');

  form1.Memo1.Append( 'Item1:');
  form1.Memo1.Append( item1_name  );
  form1.Memo1.Append( '');

  form1.Memo1.Append( 'Item2:');
  form1.Memo1.Append( item2_name );
  form1.Memo1.Append( '');

  form1.Memo1.Append( 'C225 Mary:' );
  form1.Memo1.Append( Mary_comment );
  form1.Memo1.Append( '');

  form1.Memo1.Append( 'Captain MoonWolf:' );
  form1.Memo1.Append( MoonWolf_Comment );
  form1.Memo1.Append( '======================');

  Form1.Label3.Font.Color := clYellow ;
  Form1.Label3.Visible := true ;
  Form1.Label3.top     := 45 ;
  Form1.Label3.left    := 100 ;
  Form1.Label3.Caption := '             << GAME OVER >>' + #13#10
                          +   #13#10
                          +  'YOUR SCORE = ' +  IntToStr(AIM_SCORE[1].Score)  + #13#10
                          +  'Item1 = ' + Item1_name +  #13#10
                          +  'Item2 = ' + Item2_name +  #13#10
                          +  'C225 Mary:' + Mary_Comment +  #13#10
                          +  'Captain MoonWolf：' + MoonWolf_Comment +  #13#10
                          +   #13#10
                          +   '         HIT SPACE KEY  to  RESTART !!'        ;

  AIM_SCORE[1].Score := 0; // Score initialize

end;

Procedure AIM_Judgement ;  // HIt Judgement procedure
begin

  if (
        ( abs( Form1.Image2.Left + 25 - AIM_SCORE[1].x ) < 25 )
    and
        ( abs( Form1.Image2.Top + 25 - AIM_SCORE[1].y) < 30 ) ) = true then

    begin
                                              // In case of HIT
      Form1.Label3.Font.Color := clYellow;    // Display Nice in yellow
      Form1.Label3.Visible := true;
      Form1.Label3.Left := AIM_SCORE[1].x;
      Form1.Label3.top := AIM_SCORE[1].y + 20;
      AIM_SCORE[1].count := 30;
      Form1.Label3.caption :='Nice!!' +#13#10 + '+' + inttostr(GM_Level*10);
      AIM_SCORE[1].Score := AIM_SCORE[1].Score + GM_Level*10 ;    // score addition logic
      Form1.Label4.Caption := 'Score = ' + IntToStr( AIM_SCORE[1].Score );
      Form1.Memo1.Append('Hit!:( ' + IntToStr(X) + ',' + IntToStr(Y)+' ) ' + IntToStr(GM_Level*10)+' added');

    end else
    begin
                                                // In case of Miss
      Form1.Label3.Font.Color := clRed;         // Display Oh,No .. in Red
      Form1.Label3.Visible := true;
      Form1.Label3.Left := AIM_SCORE[1].x;
      Form1.Label3.top := AIM_SCORE[1].y + 20;
      AIM_SCORE[1].count := 30;
      Form1.Label3.caption :='Oh,No..' +#13#10 + '-10';

      AIM_SCORE[1].Score := AIM_SCORE[1].Score - 10 ;    // Score deduction logic
      Form1.Label4.Caption := 'Score = ' + IntToStr( AIM_SCORE[1].Score );
      Form1.Memo1.Append('Miss!:( ' + IntToStr(X) + ',' + IntToStr(Y)+' ) ' + '- 10 deduction');

    end;
end;

Procedure Label2_Display ;       // GM_Lv, y=f(x), X,Y coordinate display on Label2
begin

  Form1.Label2.caption := '<< Game Level=' + IntToStr( GM_Level )
                                  + '>>    [  function ：　' + Fx_name  + '  ]'
                                  + '  ( X = ' + IntToStr(X) + ' '
                                  + '    Y = ' + IntToStr(Y) + ' )';

end;

procedure TForm1.FormCreate(Sender: TObject);
                                         //Game initialization by Form1 creation
begin

  KeyPreview               := True;      // Key input requires this!
  GM_Level                 := 1;         // Set Game Level = 1
  X                        := -360;
  TATE_Bar[1].visible_flag := 0 ;        // Initialize vertical square 1 (bottom left)　
  TATE_Bar[2].visible_flag := 0 ;        // Initialize vertical square 2 (bottom right)　
  AIM_SCORE[1].count       := 0;     // Nice!! display count when determining a hit
  Label3.Visible           := false;     // not display Nice!!
  Panel2.Caption           := '';        //
  Image3.Visible           := false;     // Not display small boll(image3)
  Label4.Caption           := 'Score ='; //
  Image5.Visible           := false;     // Not display horizontal square(Image5)
  Label1.Caption           := 'message window';
  Image2.Visible           := true;      // Display Star shape object(Image2)
  Form1.Caption            := 'Math Function★ Shooting ' + Version + '    Created by MoonWolf 2023';
  Memo1.Caption            := '<Math Function★ Shooting '+ Version + '>';

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState  );
  // Processing at form1 key down event

begin
  //  This is for LAZARUS(PASCAL) game developer
  //  How to identify Key typing. Following 3 points are required
  //  1. add LCLType in uses
  //  2. add KeyPreview  := True; in form1 onCreate
  //  3. use like below , you can find another VF Keys on internet
  //
  //  if (Key = VK_RIGHT)or(Key = VK_p) then
  //  image3.Left := image3.Left + 1;
  //
  //  if Key = VK_LEFT then
  //  image3.Left := image3.Left - 1;
  //
  //  if Key = VK_UP then
  //  image3.top := image3.Top - 1;
  //
  //  if Key = VK_DOWN then
  //  image3.top := image3.Top + 1;
  ////////////////////////////

  if Key = VK_p then                    // Image2 (star) warp for developers
    X := 300 ;

  if ( Key = VK_SPACE ) and ( GM_Level = 11 ) then // After GAME OVER, ReStart
    begin

      // Initialize the program for restart

      GM_Level                 := 1;         // Set Game Level = 1
      X                        := -360;
      TATE_Bar[1].visible_flag := 0 ;        // Initialize vertical square 1 (bottom left)　
      TATE_Bar[2].visible_flag := 0 ;        // Initialize vertical square 2 (bottom right)　
      AIM_SCORE[1].count       := 0;         // Nice!! display count when determining a hit
      Label3.Visible           := false;     // not display Nice!!
      Panel2.Caption           := '';        //
      Image3.Visible           := false;     // Not display small boll(image3)
      Label4.Caption           := 'Score ='; //
      Image5.Visible           := false;     // Not display horizontal square(Image5)
      Label1.Caption           := 'message window';
      Image2.Visible           := true;      // Display Star shape object(Image2)
      Form1.Caption            := 'Math Function★ Shooting ' + Version + '    Created by MoonWolf 2023';
      Memo1.Caption            := '<Math Function★ Shooting '+ Version + '>';

    end;
end;

procedure TForm1.Image1Click(Sender: TObject);
                 //image1 (XY coordinate screen) click processing
var
  pt: TPoint;

begin

  //　<For Developer : mouse click analysis>
  //
  // memo1.Append('<Get coordinates on mouse click>');
  //
  // pt := Mouse.CursorPos;     // windows whole screen mouse pointer coordinates
  // memo1.Append('Windows :: mouse cursorPos pt.x='+inttostr(pt.x)+',pt.y='+inttostr(pt.y));
  //
  // pt := ScreenToClient(pt);  // Mouse pointer coordinates in form1
  // memo1.Append('from1 :: screenToClient x='+inttostr(pt.x)+' y='+inttostr(pt.y));
  //
  ////////////////////

  pt := Mouse.CursorPos;          // No.1 Calculate coordinates on Windows Display
  pt := ScreenToClient(pt);       // No.2 Calculate the coordinates on Form1
  // ** Calculate No.2 using the calculation result of No.1. No.2 alone does not work.　

  if TATE_Bar[1].visible_flag =0 then
    begin

     x_mouse := pt.x;       // Saving Form1 X coordinate on click
     y_mouse := pt.y;       // Saving Form1 Y coordinate on click

     TATE_Bar[1].x_bar_star := x_mouse - 10 ;   // Saving Form1 X coordinate on click
     TATE_Bar[1].y_bar_star := 384 - y_mouse ;  // Saving Form1 Y coordinate on click
     TATE_Bar[1].count := bar_star_step ;
     TATE_Bar[1].x_delta := round(TATE_Bar[1].x_bar_star / bar_star_step) ;
     TATE_Bar[1].y_delta := round(TATE_Bar[1].y_bar_star / bar_star_step) ;

     TATE_Bar[2].x_bar_star := 720 - x_mouse  ; // Difference:star and vertical square(X)
     TATE_Bar[2].y_bar_star := 384 - y_mouse ;  // Difference:star and vertical square(Y)
     TATE_Bar[2].count := bar_star_step ;
     TATE_Bar[2].x_delta := round(TATE_Bar[2].x_bar_star / bar_star_step) ;
     TATE_Bar[2].y_delta := round(TATE_Bar[2].y_bar_star / bar_star_step) ;

     TATE_Bar[1].visible_flag := 1;
     TATE_Bar[2].visible_flag := 1;

     image3.Visible  := true;     // Display Image3 (small circle) on click
     image3.left     := x_mouse - 10 ; // Fine adjustment for small circle(x)
     image3.top      := y_mouse - 4 ;  // Fine adjustment for small circle(x)
     AIM_SCORE[1].X  := x_mouse - 7;   // Assign to X of AIM_SCORE (hit judgment)
     AIM_SCORE[1].Y  := y_mouse - 1 ;  // Assign to X of AIM_SCORE (hit judgment)

    end;
end;

procedure TForm1.Image2Click(Sender: TObject);
//   Apply the same reaction to clicking image1 (XY coordinates) to Image2 (star)
//   So the comment is same to above procedure TForm1.Image1Click
var
  pt: TPoint;

begin

  pt := Mouse.CursorPos;
  pt := ScreenToClient(pt);

  if TATE_Bar[1].visible_flag =0 then
    begin

     x_mouse := pt.x;
     y_mouse := pt.y;

     TATE_Bar[1].x_bar_star := x_mouse - 10 ;
     TATE_Bar[1].y_bar_star := 384 - y_mouse ;
     TATE_Bar[1].count := bar_star_step ;
     TATE_Bar[1].x_delta := round(TATE_Bar[1].x_bar_star / bar_star_step) ;
     TATE_Bar[1].y_delta := round(TATE_Bar[1].y_bar_star / bar_star_step) ;

     TATE_Bar[2].x_bar_star := 720 - x_mouse ;
     TATE_Bar[2].y_bar_star := 384 - y_mouse ;
     TATE_Bar[2].count := bar_star_step ;
     TATE_Bar[2].x_delta := round(TATE_Bar[2].x_bar_star / bar_star_step) ;
     TATE_Bar[2].y_delta := round(TATE_Bar[2].y_bar_star / bar_star_step) ;

     TATE_Bar[1].visible_flag := 1;
     TATE_Bar[2].visible_flag := 1;

     image3.Visible  := true;
     image3.left     := x_mouse - 10 ;
     image3.top      := y_mouse - 4 ;
     AIM_SCORE[1].X  := x_mouse - 7 ;
     AIM_SCORE[1].Y  := y_mouse - 1 ;

    end;
end;


procedure TForm1.Panel1Click(Sender: TObject);    
  // <For Developer only, This is not used in this game.>
  // var
  // pt: TPoint;

begin
  // pt := Mouse.CursorPos;       // mouse click coordinates for Windows's screen
  // memo1.Append('PANEL1　Windows :: mouse cursorPos pt.x='+inttostr(pt.x)+',pt.y='+inttostr(pt.y));
  //
  // pt := ScreenToClient(pt);    // mouse click coordinates for Form1
  // memo1.Append('PANEL1　from1 :: screenToClient x='+inttostr(pt.x)+' y='+inttostr(pt.y));
  //
  /////////////////////////////////////
end;

procedure TForm1.Timer1Timer(Sender: TObject);
// Timer1 : Game level management, calculate star movement with function

begin

  If X > 360 then
    // When the star moves to the right,
    // move the star back to the left and increase the game level by 1

    begin
      X := -360 ;

      if GM_Level < 11 then
        begin
          GM_Level := GM_Level + 1 ;
        end;

      If GM_Level = 11 then  // GAME_OVER Judgment of start of processing
        begin

          GAME_OVER_COMMENT;        // GAME_OVER_COKMMENT procedure
          Image2.Visible := false;
          Label2.Caption := '';     // avoid displaying GmLV11 at game over

        end;
    end;

  if ( ( X = -360 ) and ( GM_Level < 11 ) ) = true then    // GmVL to memo1
    begin
         Memo1.Append('[ Game Level ' + IntToStr( GM_Level ) + ' ]');
    end;

  If GM_Level < 11 then    // limit to move stars for GM_level 1 to 10
    begin

      //Move the star 5 pixels to the right.
      //The timing of movement depends on the Interval setting of Timer1.

      X := X + 5 ;

    end;

  // Calculate the movement of stars using mathematical functions from GmLv 1-10

  if GM_Level=1 then
    begin
      if X<>0 then
        begin
          Y := round( 3500 * 1 / X )*(-1);
          Fx_name := 'Y = - 3500 * 1/x';
        end;
    end;

  if GM_Level=2 then
    begin
      Y := abs( round( 0.3 * X + 30 ) );
      Fx_name := 'Y = ┃　0.3X + 30　┃';
    end;

  if GM_Level=3 then
    begin
      if X<>0 then
        begin
          Y := round( 3500 * 1 / X );
          Fx_name := 'Y = 3500 * 1 / X';
        end;
    end;

  if GM_Level=4 then
    begin
      Y := round( 0.003 * X * X )*(-1) ;
      Fx_name := 'Y = - 0.003 * X ^2' ;
    end;

  if GM_Level=5 then
    begin
      Y := round( 0.007 * ( X-50 ) * ( X-50) - 150 ) ;
      Fx_name := 'Y = 0.007 * ( X - 50 )^2 - 150' ;
    end;

  if GM_Level=6 then
    begin
      Y := round( 0.006 * ( X+70 ) * ( X+70) - 200 )*(-1) ;
      Fx_name := 'Y = - 0.006 * ( X + 70 )^2 + 200' ;
    end;

  if GM_Level=7 then
    begin
      y:= round( 120*sin(x*0.05) );
      Fx_name := 'Y = 120 * sin( X * 0.05 )' ;
    end;

  if GM_Level=8 then
    begin
      y:= round( 10*sin(x*0.0098)/cos(x*0.0098) )  ;
      Fx_name := 'Y = 10 * tan( X * 0.098 )' ;
    end;


  if GM_Level=9 then
    begin
      y:=round(x*sin(x*0.05));
      Fx_name := 'Y = X * sin( X * 0.05 )' ;
    end;

  if GM_Level=10 then
    begin
      if X <> 0 then
        begin
          y:= round( 5000/x *sin(x*0.05) );
          Fx_name := 'Y = 5000/X * sin( X * 0.05 )' ;
        end;
    end;

  Image2.Left := x_cnt + X-25;  // Image2 Center correction (X coordinate)
  Image2.Top  := y_cnt - Y-25;  // Image2 Center correction (Y coordinate)

  If GM_Level < 11 then
    begin
      Label2_display;
    end;
end;

procedure TForm1.Timer2Timer(Sender: TObject); // Display control of SuperEleMag
begin

  // process :vertical square (bottom left) approaches the clicked coordinates

  if ( ( TATE_Bar[1].visible_flag =1 ) and ( TATE_Bar[1].count > 0  ) ) = true then
    begin
      image4.Left  := image4.Left + TATE_Bar[1].x_delta;
      image4.Top   := image4.Top  - TATE_Bar[1].y_delta;

      image4.Height := round( image4.Height *  (TATE_Bar[1].count+5) / bar_star_step ) ;
      image4.Top   := image4.Top  +4 ;

      TATE_Bar[1].count := TATE_Bar[1].count - 1;

      if TATE_Bar[1].count=0 then
        begin
          TATE_Bar[1].visible_flag :=0;  // back to original position
          Image4.Left := 5;
          Image4.top  := 300;
          Image4.Height := 200;
          Image4.Width := 20;
          Image3.visible := false;

          AIM_Judgement;            // Goto Hit judgement process
        end;
    end;

  // process :vertical square (bottom right) approaches the clicked coordinates

  if ( ( TATE_Bar[2].visible_flag =1 ) and ( TATE_Bar[2].count > 0  ) ) = true then
    begin
      image6.Left  := image6.Left - TATE_Bar[2].x_delta;
      image6.Top   := image6.Top  - TATE_Bar[2].y_delta;

      image6.Height := round( image6.Height *  (TATE_Bar[2].count+5) / bar_star_step ) ;
      image6.Top   := image6.Top  +4 ;

      TATE_Bar[2].count := TATE_Bar[2].count - 1;

      if TATE_Bar[2].count=0 then
        begin
          TATE_Bar[2].visible_flag :=0;   // back to original position
          Image6.Left := 704;
          Image6.top  := 296;
          Image6.Height := 200;
          Image6.Width := 20;
         end;
    end;

  // Control of “Nice!!” display time after hit determination
  if AIM_SCORE[1].count > 0 then
    begin
      AIM_SCORE[1].count := AIM_SCORE[1].count - 1;
       if AIM_SCORE[1].count = 0 then
         begin
           Label3.Visible := false;
         end;
    end;
end;

end.
