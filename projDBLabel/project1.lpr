program project1; 

{$mode delphi}{$H+}

uses
  Interfaces, Forms, Unit1, ucds;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWForm1, WForm1);
  Application.Run;
end.


{@PAS2JS_BEGIN}
{
  "Compiler" : "C:\\fpc2js\\pas2js_widgets\\pas2js\\win32\\pas2js.exe",
  "Output" : "",
  "Template" : "",
  "Browser" : "C:\\Arquivos de programas\\Mozilla Firefox\\firefox.exe",
  "CustomOptions" : [
    "-Jirtl.js",
    "-Tbrowser",
    "-MDelphi",
    "-Jc",
    "-Jminclude",
    "-O1"
  ]
}
{@PAS2JS_END}

