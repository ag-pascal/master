unit UForm3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS, Web,
  JComponents,
  //pas2js.Element, pas2js.Form, pas2js.application,
  uWButton
  {Forms, Controls, Graphics, Dialogs};

type
  TForm3 = class(TWForm)
  private
    button1: TButton;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  public

  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

procedure TForm3.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  console.log('TForm3.InitializeForm');
end;

procedure TForm3.InitializeObject;

  procedure Button1OnClick(sender: TObject);
  begin
    console.log('TForm3 button3 clicked');
    //Application.GoToForm('Form1');
  //  Application.GoToForm('Form1');
  end;

  procedure rolaPreta(aTime: TJSDOMHighResTimeStamp);
  begin
    console.log('Form3',  aTime);
    ///console.log(Self);
    button1.Handle.style.setProperty('width', 'calc(100% - 20px)');
    button1.Handle.style.setProperty('height', '50px');
  end;

begin
  inherited;
  console.log('TForm3.InitializeObject');

  button1:= TButton.Create(Self);
  button1.SetBounds(0, 30, 250, 250);
  button1.Caption:='Btn3';
//  button1.SetInnerHTML('Button3');
  button1.OnClick := @Button1OnClick;
  button1.ReadyExecute(@rolapreta);
end;

procedure TForm3.Resize;
begin
  inherited;
  console.log('TForm3.Resize');
end;

end.

