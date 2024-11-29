// MoonWolf , Todolist Lv.10

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, DBCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DataSource4: TDataSource;
    DataSource5: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    DBGrid5: TDBGrid;
    DBNavigator1: TDBNavigator;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLQuery3: TSQLQuery;
    SQLQuery4: TSQLQuery;
    SQLQuery5: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLQuery5AfterDelete(DataSet: TDataSet);
    procedure SQLQuery5AfterPost(DataSet: TDataSet);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

Procedure ResetDisplay;
begin
  // Refresh display for DBGrid1 to DBGrid5
  // For Main Part
  Form1.SQLQuery5.Close;
  Form1.SQLQuery5.SQL.Text := 'SELECT * FROM ToDoList10';
  Form1.SQLQuery5.Open;

  // For Area 1
  Form1.SQLQuery1.Close;
  Form1.SQLQuery1.SQL.Text := 'SELECT * FROM todolist10 where imp="high" and urg="high"';
  Form1.SQLQuery1.Open;

  // For Area 2
  Form1.SQLQuery2.Close;
  Form1.SQLQuery2.SQL.Text := 'SELECT * FROM todolist10 where imp="low" and urg="high"';
  Form1.SQLQuery2.Open;

  // For Area 3
  Form1.SQLQuery3.Close;
  Form1.SQLQuery3.SQL.Text := 'SELECT * FROM todolist10 where imp="low" and urg="low"';
  Form1.SQLQuery3.Open;

  // For Area 4
  Form1.SQLQuery4.Close;
  Form1.SQLQuery4.SQL.Text := 'SELECT * FROM todolist10 where imp="high" and urg="low"';
  Form1.SQLQuery4.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //SQLQuery5.SQL.Text := 'select * from todolist10';
  //SQLQuery5.Open;

  ResetDisplay; // Refresh DBGrid1 to DBGrid5
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  SQLQuery5.Close;

  // INSERT by Parameter Query

  SQLQuery5.SQL.Text :=
    'INSERT INTO ToDoList10 (Imp, Urg, ToDo) VALUES (:Imp, :Urg, :ToDo)';

  SQLQuery5.Params.ParamByName('Imp').AsString := ComboBox1.Text;
  SQLQuery5.Params.ParamByName('Urg').AsString := ComboBox2.Text;
  SQLQuery5.Params.ParamByName('ToDo').AsString := Edit1.Text;
  SQLQuery5.ExecSQL;

  SQLTransaction1.Commit;

  ResetDisplay; // Refresh DBGrid1 to DBGrid5

end;

procedure TForm1.SQLQuery5AfterDelete(DataSet: TDataSet);
begin
  SQLQuery5.ApplyUpdates;
  SQLTransaction1.CommitRetaining;
  ResetDisplay; // Refresh DBGrid1 to DBGrid5
end;

procedure TForm1.SQLQuery5AfterPost(DataSet: TDataSet);
begin
  SQLQuery5.ApplyUpdates;
  SQLTransaction1.CommitRetaining;
  ResetDisplay; // Refresh DBGrid1 to DBGrid5
end;

end.
