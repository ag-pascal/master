program project1;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, JComponents,
  uForm1, uForm2, uForm3, uForm4, uStore1;

type
  { TApplication }
  TApplication = class(TCustomApplication)
  private
    {  private declarations  }
  protected
    procedure ApplicationStarting; override;
  end;

{ TApplication }

procedure TApplication.ApplicationStarting;
begin
  inherited ApplicationStarting;

  { Register Auto Create Forms }
  CreateForm('Form1', TForm1);
  CreateForm('Form2', TForm2);
  CreateForm('Form3', TForm3);
end;

//var
 // Application: TCustomApplication;

begin
  try
    Application := TApplication.Create;
    Application.RunApp;
  except
    on e: Exception do
      console.log(e.Message);
  end;
end.
