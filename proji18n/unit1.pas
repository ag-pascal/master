unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EDuplicate = class(Exception);

  TDataType = (dtUnknown, dtString, dtBoolean, dtInteger, dtFloat, dtDate,
      dtTime, dtDateTime, dtBlob);

type
   { TFormatSettings }
   TFormatSettings = class
   private
     FTranslations: TStrings;
     procedure SetTranslations(Value: TStrings);
   protected
     procedure SetupTranslations;
   public
     constructor Create;
   published
     property Translations: TStrings read FTranslations write SetTranslations;
   end;

function DataTypeToStr(Value: TDataType): string;
function Translate(const ID: string): string; overload;
function Translate(const ID: string; const Values: array of string): string; overload;
function TranslateRS(const Msg: string; const Args: array of JSValue): String; overload;

var
  FormatSettings: TFormatSettings = nil;

ResourceString
  DuplicateMsg   = 'An item with key %0:s already exists';
  KeyNotFoundMsg = 'Method: %0:s key [''%1:s''] not found in container';
  NotEmptyMsg    = 'Hash table not empty.';
  SErrNoSuchItem = 'No item in list for %p';
  SDuplicateItem = 'Item already exists in list: %p';

implementation

{ TFormatSettings }

constructor TFormatSettings.Create;
begin
  inherited Create;
  FTranslations := TStringList.Create;
  SetupTranslations;
end;


procedure TFormatSettings.SetupTranslations;
begin
  FTranslations.BeginUpdate;
  try
    FTranslations.Add('DuplicateMsg=An item with key %0:s already exists');
    FTranslations.Add('TYPE_UNKNOWN=Unknown');
    FTranslations.Add('TYPE_STRING=String');
    FTranslations.Add('TYPE_BOOLEAN=Boolean');
    FTranslations.Add('TYPE_INTEGER=Integer');
    FTranslations.Add('TYPE_FLOAT=Float');
    FTranslations.Add('TYPE_DATE=Date');
    FTranslations.Add('TYPE_TIME=Time');
    FTranslations.Add('TYPE_DATETIME=DateTime');
    FTranslations.Add('TYPE_BLOB=Blob');
    FTranslations.Add('TYPE_SYMBOL=Symbol');
    FTranslations.Add('DLG_MSG=Message');
    FTranslations.Add('DLG_BTN_OK=OK');
    FTranslations.Add('DLG_BTN_CANCEL=Cancel');
    FTranslations.Add('DLG_BTN_ABORT=Abort');
    FTranslations.Add('DLG_BTN_RETRY=Retry');
    FTranslations.Add('DLG_BTN_IGNORE=Ignore');
    FTranslations.Add('DLG_BTN_YES=Yes');
    FTranslations.Add('DLG_BTN_NO=No');
    FTranslations.Add('DLG_BTN_ALL=All');
    FTranslations.Add('DLG_BTN_NOTOALL=No to All');
    FTranslations.Add('DLG_BTN_YESTOALL=Yes to All');
    FTranslations.Add('DLG_BTN_CLOSE=Close');
    FTranslations.Add('ERR_CMP_DESTROY="%s" component already destroyed');
    FTranslations.Add('ERR_LOAD_PERSISTENT=Persistent load error (%s)');
    FTranslations.Add('ERR_LOAD_METHOD=Method %s not found');
    FTranslations.Add('ERR_SAVE_PERSISTENT=Persistent save error (%s)');
    FTranslations.Add('ERR_SET_RANGE=Value "%s" out of range for set');
    FTranslations.Add('ERR_BOOLEAN_LITERAL=Invalid boolean literal "%s" specified');
    FTranslations.Add('ERR_FORMAT=Error in the format string %s (%s)');
    FTranslations.Add('ERR_VALUE_CONVERT=Error converting %s value to %s value');
    FTranslations.Add('ERR_VALUE_READONLY="%s" is read-only and cannot be modified');
    FTranslations.Add('ERR_DATETIME_DATE=date');
    FTranslations.Add('ERR_DATETIME_TIME=time');
    FTranslations.Add('ERR_DATETIME_MONTH=Invalid month %s specified');
    FTranslations.Add('ERR_DATETIME_DAY=Invalid day %s specified');
    FTranslations.Add('ERR_DATETIME_TOOMANYCOMPS=Too many %s components');
    FTranslations.Add('ERR_DATETIME_MISSINGCOMP=Missing %s component');
    FTranslations.Add('ERR_DATETIME_INVALIDCOMP=Invalid %s component');
    FTranslations.Add('ERR_DATETIME_INVALIDFORMAT=Invalid %s format');
    FTranslations.Add('ERR_DATETIME_INVALID=Invalid %s (%s)');
    FTranslations.Add('ERR_PARSE_TERMSTR=Unterminated string at %s');
    FTranslations.Add('ERR_PARSE_MISSING=Missing %s');
    FTranslations.Add('ERR_PARSE_EXPECT=Expected %s, instead found %s at %s');
    FTranslations.Add('ERR_LIST_BOUNDS=List index %s out of bounds');
    FTranslations.Add('ERR_LIST_SORT=You can only use the Find method when a list is sorted');
    FTranslations.Add('ERR_OWNER=Invalid owner class %s passed to constructor');
    FTranslations.Add('ERR_LOADUI_STATE=Error loading interface state (%s)');
    FTranslations.Add('ERR_UI_DUPSTATE=The interface state %s already exists');
    FTranslations.Add('ERR_UI_ELEMENTCLASS=Cannot find registered element class information '
      +
      'for the %s class');
    FTranslations.Add('ERR_DOM_EVENTADD=Cannot add event handler to "%s" element for "%s" event');
    FTranslations.Add('ERR_DOM_EVENTCLEAR=Cannot remove "%s" event handler from "%s"');
    FTranslations.Add('ERR_HTTP_REQUEST=Error executing request "%s" (%s)');
    FTranslations.Add('ERR_DATA_DUPCOL=The "%s" column already exists');
    FTranslations.Add('ERR_DATA_COLNAME=Column names cannot be blank (%s)');
    FTranslations.Add('ERR_DATA_COLTYPE=Unknown column type (%s)');
    FTranslations.Add('ERR_DATA_COLLENGTH=Invalid "%s" column length %s');
    FTranslations.Add('ERR_DATA_COLSCALE=Invalid "%s" column scale %s');
    FTranslations.Add('ERR_DATA_CONNECT=Cannot connect to server');
    FTranslations.Add('ERR_DATA_LOADCODE=Status code %s');
    FTranslations.Add('ERR_DATA_LOAD=Dataset load response error (%s)');
    FTranslations.Add('ERR_DATA_COMMIT=Database commit response error (%s)');
    FTranslations.Add('ERR_DATA_TRANSACTIVE=A transaction is not active');
    FTranslations.Add('ERR_DATA_PENDREQUEST=There are no pending requests');
    FTranslations.Add('ERR_DATA_COLUMNS=At least one column must be defined for the "%s" dataset');
    FTranslations.Add('ERR_DATA_OPEN=The "%s" dataset must be open in order to complete this operation');
    FTranslations.Add('ERR_DATA_NOTOPEN=The "%s" dataset cannot be open when completing this operation');
    FTranslations.Add('ERR_DATA_NOTEDITING=The "%s" dataset must be in an editable mode before a column '
      +
      'can be assigned a value');
    FTranslations.Add('ERR_DATA_TRANSCLOSE=Cannot close the "%s" dataset while there are still '
      +
      'active transaction operations for the dataset');
    FTranslations.Add('ERR_DATA_FINDMODE=The "%s" dataset is not in Find mode');
    FTranslations.Add('ERR_DATA_FINDNEAR=You can only search for nearest matches in the "%s" dataset '
      +
      'when searching on columns that match the current sort order');
    FTranslations.Add('ERR_DATA_COLNOTFOUND=Column "%s" not found');
    FTranslations.Add('ERR_DATA_TRANSDATASET=Invalid dataset "%s" specified in the transaction operations');
    FTranslations.Add('ERR_DATA_TRANSOPTYPE=Invalid or unknown operation type %s specified in the transaction operations');
    FTranslations.Add('ERR_DATA_DATASETDB=The "%s" dataset cannot be loaded using the "%s" database');
    FTranslations.Add('ERR_APP_ERRORLINE=Line: %s');
    FTranslations.Add('ERR_APP_ERRORTITLE=Application Error');
    FTranslations.Add('APP_LOAD_MESSAGE=Loading %s...');
    FTranslations.Add('ERR_DLG_BUTTONS=You must specify at least one button for the message dialog');
    FTranslations.Add('ERR_FORM_SHOWMODAL=You cannot call ShowModal for the embedded form %s');
    FTranslations.Add('ERR_CTRL_PARENT=The %s control cannot be a parent of the %s control');
    FTranslations.Add('ERR_CALENDAR_COLINDEX=Column index %s out of bounds');
    FTranslations.Add('ERR_CALENDAR_ROWINDEX=Row index %s out of bounds');
    FTranslations.Add('ERR_GRID_COLINDEX=Column index %s out of bounds');
    FTranslations.Add('ERR_GRID_ROWINDEX=Row index %s out of bounds');
    FTranslations.Add('ERR_GRID_COLNOTFOUND=Column "%s" not found');
    FTranslations.Add('ERR_CANVAS=Your browser does not have HTML5 canvas support');
    FTranslations.Add('ERR_STORAGE=Your browser does not have HTML5 persistent storage support');
    FTranslations.Add('ERR_SCRIPT_LOAD=Your browser does not support dynamic script loading');
    FTranslations.Add('ERR_MEDIA=Your browser does not have HTML5 media support');
    FTranslations.Add('ERR_MAP=The map API has not been loaded');
    FTranslations.Add('ERR_MAP_GEOCODE=Geocoding request error "%s"');
    FTranslations.Add('ERR_MAP_LOCNOTFOUND=Location "%s" not found');
    FTranslations.Add('ERR_MAP_DUPLOC=The "%s" location already exists');
    FTranslations.Add('ERR_MAP_LOCNAME=Location names cannot be blank (%s)');
    FTranslations.Add('ERR_SIZER_CONTROL=The sizer control itself cannot be assigned as the target control');
    FTranslations.Add('ERR_ZOOM_FACTOR=Zoom factor %s invalid, factor must be between 1 and 100');
    FTranslations.Add('ERR_SLIDE_COUNT=At least %s slide images must be specifed before the slide '
      +
      'show can be started');
  finally
    FTranslations.EndUpdate;
  end;
end;

procedure TFormatSettings.SetTranslations(Value: TStrings);
begin
  FTranslations.Assign(Value);
end;

function DataTypeToStr(Value: TDataType): string;
begin
  case Value of
    dtUnknown:
      Result := Translate('TYPE_UNKNOWN');
    dtString:
      Result := Translate('TYPE_STRING');
    dtBoolean:
      Result := Translate('TYPE_BOOLEAN');
    dtInteger:
      Result := Translate('TYPE_INTEGER');
    dtFloat:
      Result := Translate('TYPE_FLOAT');
    dtDate:
      Result := Translate('TYPE_DATE');
    dtTime:
      Result := Translate('TYPE_TIME');
    dtDateTime:
      Result := Translate('TYPE_DATETIME');
    dtBlob:
      Result := Translate('TYPE_BLOB');
  else
    Result := '';
  end;
end;

function TranslateRS(const Msg: string; const Args: array of JSValue): String;
begin
  Result:= Format(Msg,Args);
end;

function Translate(const ID: string): string;
begin
Result:= FormatSettings.Translations.Values[ID];
end;

function Translate(const ID: string; const Values: array of string): string;
begin
  Result := Format(Translate(ID), Values);
end;

initialization
  FormatSettings := TFormatSettings.Create;

end.

