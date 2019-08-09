unit UForm4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS, Web,
  JComponents, uWButton
  {Forms, Controls, Graphics, Dialogs};

type

  { TForm4 }

  TForm4 = class(TWForm)
    Btn1: TButton;
    procedure Btn1Click(Sender: TObject);
  private
    button1: TButton;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
    procedure LoadDFMValues; override;
  public

  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

procedure TForm4.Btn1Click(Sender: TObject);
begin
  console.log('TForm4.Btn1Click');
  //Application.GoToForm('Form3');
end;

procedure TForm4.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  console.log('TForm4.InitializeForm');
end;

procedure TForm4.InitializeObject;
  procedure Button1OnClick(sender: TObject);
  begin
    console.log('TForm4 button1 clicked');
    //Application.GoToForm('Form1');
  end;

  procedure rolaPreta(aTime: TJSDOMHighResTimeStamp);
  begin
    console.log('Form4',  aTime);
    //console.log(Self);
  end;

begin
  inherited;
  console.log('TForm4.InitializeObject');

  button1:= TButton.Create(Self);
  button1.SetBounds(10, 30, 100, 50);
  button1.Caption:='Btn4';
//  button1.SetInnerHTML('Button1');
  button1.OnClick := @Button1OnClick;
  button1.ReadyExecute(@rolapreta);
end;

procedure TForm4.Resize;
begin
  inherited;
  console.log('TForm4.Resize');
end;

procedure TForm4.LoadDFMValues;
begin
inherited;
  {$I Form4.lfm.inc}
end;

end.

