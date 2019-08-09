unit uMain;

{$MODE objfpc}{$H+}

interface

uses
  uinternationalization,
  Classes, SysUtils;

type
  TApplication = class(TObject)
  private
    {  private declarations  }
    i18n1: TJSi18n;
    Application: TApplication;
  public
    procedure RunApp; virtual;
  end;

implementation

procedure TApplication.RunApp;
begin
  i18n1:= TJSi18n.Create;
  i18n1.InitializeObject;
end;

end.
