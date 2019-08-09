unit UForm2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS, Web,
  JComponents, uWButton//, DB, JSONDataSet, uCDS
  {Forms, Controls, Graphics, Dialogs};

type
  TForm2 = class(TWForm)
  private
    button1: TButton;
    (*
    FDataLink: TFieldDataLink;
    Connection1: TConnection;
    DataSet1: TDataSet;
    DataSource1: TWDataSource;
   *)
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  public

  end;

var
  Form2: TForm2;

implementation

uses
  uMain;

{$R *.lfm}

procedure TForm2.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  console.log('TForm2.InitializeForm');


end;

procedure TForm2.InitializeObject;
  procedure Button1OnClick(sender: TObject);
  begin
    console.log('TForm2 button clicked');
    Application.GoToForm('Form1');
  end;

  procedure rolaPreta(aTime: TJSDOMHighResTimeStamp);
  begin
    console.log('Form2',  aTime);

    button1.Handle.style.setProperty('color' , 'white');
    button1.Handle.style.setProperty('color' , 'white');
    button1.Handle.style.setProperty('border-radius' , '4px');
    button1.Handle.style.setProperty('background' , '#699BCE');
    button1.Handle.style.setProperty('cursor' , 'pointer');
    button1.Handle.style.setProperty('box-shadow' , '0 -1px 1px 0 rgba(0, 0, 0, 0.25) inset, 0 1px 1px 0 rgba(0, 0, 0, 0.10) inset;)');

  end;

  procedure OnhandleClick;
  begin
    console.log('handleClick clicked');
  end;


(*  procedure p;
  begin
    while not DataSet1.EOF do
    begin
      console.log(
        DataSet1.FieldByName('Species_No').AsString,
        DataSet1.FieldByName('Category').AsString
      );
      DataSet1.Next;
    end;
  end;*)

begin
  inherited;
  console.log('TForm2.InitializeObject');

  button1:= TButton.Create(Self);
  button1.SetBounds(20, 20, 200, 200);
  button1.Caption:='Btn2';
  button1.OnClick := @Button1OnClick;
  button1.ReadyExecute(@rolapreta);
(*
  FDataLink := TFieldDataLink.Create;

  Connection1:=  TConnection.Create(nil);
  Connection1.Name := 'Connection1';
  Connection1.Active := false;

  DataSet1:=     TDataSet.Create(nil);
  DataSet1.Name := 'DataSet1';
  DataSet1.Connection := Connection1;

  DataSource1:=  TWDataSource.Create(nil);
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

  window.setTimeout(@p, 1000);
  *)
end;

procedure TForm2.Resize;
begin
  inherited;
  console.log('TForm2.Resize');
end;

end.

