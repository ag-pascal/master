unit Unit1;

{$mode delphi}{$H+} 

interface

uses
  JS, Web, Classes, SysUtils, Graphics, Controls, Forms, Dialogs,
  DB, JSONDataSet, WebCtrls, ucds;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    Button1: TWButton;
    DBLabel1: TDBLabel;
    FDataLink: TFieldDataLink;
    Connection1: TConnection;
    DataSet1: TDataSet;
    DataSource1: TWDataSource;

    WButton2: TWButton;
    procedure WButton1Click(Sender: TObject);
    procedure WButton2Click(Sender: TObject);
  private

  public
    procedure Loaded; override;
  end;

var
  WForm1: TWForm1;

implementation

procedure TWForm1.WButton1Click(Sender: TObject);
begin
  console.log('clicked');
end;

procedure TWForm1.WButton2Click(Sender: TObject);
begin
  DataSet1.Next;
end;

procedure TWForm1.Loaded;
begin
   inherited Loaded;
  {$I unit1.wfm}

   FDataLink := TFieldDataLink.Create;

   Connection1:=  TConnection.Create(Self);
   Connection1.Name := 'Connection1';
   Connection1.Active := false;

   DataSet1:=     TDataSet.Create(Self);
   DataSet1.Name := 'DataSet1';
   DataSet1.Connection := Connection1;

   DataSource1:=  TWDataSource.Create(Self);
   DataSource1.Name := 'DataSource1';
   DataSource1.DataSet := DataSet1;

   Connection1.URI := 'data.json';
   Connection1.DataNode := 'data';
   DataSet1.FieldDefs.Clear();


   DataSet1.FieldDefs.Add('Species_No', TFieldType.ftString,0);
   DataSet1.FieldDefs.Add('Category',  TFieldType.ftString,50);
   DataSet1.FieldDefs.Add('Common_Name', TFieldType.ftString,50);
   DataSet1.FieldDefs.Add('Species_Name', TFieldType.ftString,50);
   DataSet1.FieldDefs.Add('Length__cm_', TFieldType.ftInteger,0);
   DataSet1.FieldDefs.Add('Length_In', TFieldType.ftString,30);
   DataSet1.FieldDefs.Add('Notes', TFieldType.ftString,255);
   Connection1.Active := true;

   DBLabel1:= TDBLabel.Create(Self);
   DBLabel1.Parent := Self;
   DBLabel1.Name := 'WDBText1';
   DBLabel1.Left := 90;
   DBLabel1.Top := 7;
   DBLabel1.Width := 457;
   DBLabel1.Height := 22;
   DBLabel1.AutoSize := false;
   DBLabel1.Caption := 'WDBText1';
   DBLabel1.DataField := 'Category';
   DBLabel1.DataSource := DataSource1;
end;

end.

