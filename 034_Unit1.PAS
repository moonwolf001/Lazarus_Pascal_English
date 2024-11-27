// MoonWolf , Lazarus SQL DB
// Full function of DBGrid and DBNavigator

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, DBCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure SQLQuery1AfterDelete(DataSet: TDataSet);
    procedure SQLQuery1AfterPost(DataSet: TDataSet);
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
  // to display student table on DBGrid1
  SQLQuery1.SQL.Text := 'SELECT * FROM student';
  SQLQuery1.Open;
end;

procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);
begin
  // to delete a record
  SQLQuery1.ApplyUpdates;
  SQLTransaction1.CommitRetaining;
end;

procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);
begin
  // to modify field, or to add a record
  SQLQuery1.ApplyUpdates;
  SQLTransaction1.CommitRetaining;
end;

end.
