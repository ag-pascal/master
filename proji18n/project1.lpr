program project1;

{$mode objfpc}

uses
  uMain, Web, Classes, SysUtils, uutils;

var
  Application: TApplication;

begin
  try
    Application := TApplication.Create;
    Application.RunApp;
  except
    on e: Exception do
      console.log(e.Message);
  end;


(*
uses
  Classes, SysUtils, JS, Web, Unit1, browserconsole, umain, uinternationalization;

var TempMessage: String;
begin
  // Your code here
//  pas['Unit1'].$resourcestrings.YourResourcestring.current = 'The Translated Value';
  //resourcestrings.YourResourcestring.current = 'The Translated Value';

//  asm
//     pas['Unit1'].$resourcestrings.DuplicateMsg.current = 'The Translated Value';
//     console.log( pas['Unit1'].$resourcestrings.DuplicateMsg );
//  end;

TempMessage := Translate('ERR_DATETIME_INVALID',
    [Translate('ERR_DATETIME_TIME'), TimeToStr(now)]);

WriteLn(TempMessage);

WriteLn( DataTypeToStr(dtBoolean) );
WriteLn( Translate('ERR_CMP_DESTROY', ['button1']) );
WriteLn( Translate('APP_LOAD_MESSAGE', ['the main screen']) );
WriteLn( TranslateRS(DuplicateMsg, ['123456789']) );
WriteLn( Translate('DuplicateMsg', ['1234']) );
WriteLn( TJSJSON.stringify(EDuplicate.CreateFmt(DuplicateMsg, ['123'])) );
 *)

end.
