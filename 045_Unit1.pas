// MoonWolf , To do list Lv.1

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, DBCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Edit1: TEdit;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLQuery1AfterDelete(DataSet: TDataSet);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLQuery1.SQL.Text := 'select * from todolist';
  SQLQuery1.Open;
end;

procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);
begin
  SQLQuery1.ApplyUpdates;
  SQLTransaction1.CommitRetaining;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Adding new record from Edit1.text by pressing button1
  SQLQuery1.Close;

  SQLQuery1.SQL.Text := 'INSERT INTO ToDoList (ToDo) VALUES (:ToDo)';
  SQLQuery1.ParamByName('ToDo').AsString := Edit1.Text;

  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;

  // Refresh DBGrid Display
  SQLQuery1.SQL.Text := 'SELECT * FROM ToDoList';
  SQLQuery1.Open;
end;

end.
