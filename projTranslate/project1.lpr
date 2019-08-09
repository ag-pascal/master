program project1;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, Unit1, browserconsole;

var TempMessage: String;
begin
  // Your code here

TempMessage := Translate('ERR_DATETIME_INVALID',
    [Translate('ERR_DATETIME_TIME'), TimeToStr(now)]);

WriteLn(TempMessage);

WriteLn( DataTypeToStr(dtBoolean) );
WriteLn( Translate('ERR_CMP_DESTROY', ['button1']) );
WriteLn( Translate('APP_LOAD_MESSAGE', ['the main screen']) );
WriteLn( TranslateRS(DuplicateMsg, ['123456789']) );
WriteLn( Translate('DuplicateMsg', ['1234']) );
WriteLn( TJSJSON.stringify(EDuplicate.CreateFmt(DuplicateMsg, ['123'])) );

end.
