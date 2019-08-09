unit ucds;

{$mode delphi}

interface

uses
  Classes, SysUtils, Types, JS, Web, DB, JSONDataSet, Controls, WebCtrls;

type
  { TFieldDataLink }
  TFieldDataLink = class(TDataLink)
  private
  { private declarations }
    FOnDataChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    FField: TField;
    FFieldName: String;
    FModified: Boolean;
  protected
  { protected declarations }
    // set current field
    //procedure SetFieldName(const Value: string);
    procedure UpdateField;

    procedure ActiveChanged; override;
    //procedure EditingChanged; override;
    //procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    //procedure FocusControl(aField: TFieldRef); Override;
  public
  { public declarations }
    // for control intitiating db changes etc
    //function Edit: Boolean;
    procedure Modified;
    //procedure Reset;
  published
  { published declarations }
    // Attached control
    //property Control: TComponent read FControl write FControl;

    // Basic DB interfaces
    //property Field: TField read FField;
    property FieldName: string read FFieldName write FFieldName {SetFieldName};

    // Current State of DB
    //property CanModify: Boolean read GetCanModify;
    //property Editing: Boolean read FEditing;

    // Our Callbacks
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    //property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

type
  TControl = TCustomControl;

type
  { TWDataSource }
  TWDataSource = class(TDataSource)
  private
  { private declarations }
    FParent: TControl;
    FLeft: Integer;
    FTop: Integer;
  protected
  { protected declarations }
  public
  { public declarations }

  published
  { published declarations }
    property Parent: TControl read FParent write FParent;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
  end;

  type
    TConnectErrorEvent = procedure(Sender: TObject; ErrorCode: Integer);

type
  TDataSet = class;

type
  { TConnection }
  TConnection = class(TComponent)
  private
  { private declarations }
    FReq: TJSXMLHttpRequest;
    FDS: TDataSet;
    FParent: TControl;
    FLeft: Integer;
    FTop: Integer;
    FActive: Boolean;
    FDataNode: String;
    FURI: String;
    FAfterConnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FOnConnectError: TConnectErrorEvent;
  protected
  { protected declarations }
    procedure SetActive(AValue: boolean);
  public
  { public declarations }
    function onLoad(Event: TEventListenerEvent): boolean;
    procedure RegisterDataSet(value: TDataSet);
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoError(ErrorCode: Integer);
    constructor Create(AOwner: TComponent); override;
  published
  { published declarations }
    property Parent: TControl read FParent write FParent;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Active: Boolean read FActive write SetActive;
    property DataNode: String read FDataNode write FDataNode;
    property URI: String read FURI write FURI;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property OnConnectError: TConnectErrorEvent read FOnConnectError write FOnConnectError;
  end;

type
  { TDataSet }
  TDataSet = class(TBaseJSONDataSet)
  private
  { private declarations }
    FConnection: TConnection;
    FParent: TControl;
    FLeft: Integer;
    FTop: Integer;
  protected
  { protected declarations }
    procedure SetConnection(Value: TConnection);
    Function CreateFieldMapper : TJSONFieldMapper; override;
  public
  { public declarations }
  published
  { published declarations }
    // Can be set directly if the dataset is closed. If metadata is set, it must match the data.
    Property Rows;
    property Parent: TControl read FParent write FParent;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Connection: TConnection read FConnection write SetConnection;
  end;


type
  { TDBLabel }
  TDBLabel = class(TWLabel)
  private
  { private declarations }
    FDataLink: TFieldDataLink;
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource;
    function GetDataField: String;
  protected
  { protected declarations }
    function CheckDataSet: boolean;
    //function GetDisplayText: String; override;
    //procedure CreateInitialize; override;
  public
  { public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
  { published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{ TDBLabel }

procedure TDBLabel.SetDataField(const Value: string);
begin
  FDataLink.FFieldName := Value;
  FDataLink.UpdateField();
end;

procedure TDBLabel.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBLabel.GetDataField: String;
begin
  Result := FDataLink.FFieldName;
end;

function TDBLabel.CheckDataSet: boolean;
begin
  Result := false;
  if (((assigned(DataSource)) and DataSource.Enabled) and (GetDataField() <> '')) then
  begin
    if ((assigned(FDataLink.Dataset)) and FDataLink.Dataset.Active) then
      result := true;
  end;
end;


(*function TDBLabel.GetDisplayText: String;
begin
  if (not CheckDataSet()) then
    Result := '' else
    Result := inherited GetDisplayText
 end;*)

procedure TDBLabel.DataUpdate(Sender: TObject);
begin
// empty;
end;

procedure TDBLabel.DataChange(Sender: TObject);
begin
  if (not(assigned(FDataLink.Dataset))) then exit;
  if (not(assigned(FDataLink.FField))) then
    FDataLink.UpdateField();
  Caption := (FDataLink.FField.DisplayText);
end;

procedure TDBLabel.ActiveChange(Sender: TObject);
begin
  if (assigned(FDataLink.Dataset)) then
  begin
    if (not FDataLink.Dataset.Active or  not DataSource.Enabled) then
      Caption := ('') else
      DataChange(Self);
  end;
end;

(*procedure TDBLabel.CreateInitialize;
begin
  inherited; //CreateInitialize;

  FDataLink := TFieldDataLink.Create;
  FDataLink.FOnUpdateData := @DataUpdate;
  FDataLink.FOnDataChange := @DataChange;
  FDataLink.FOnActiveChange := @ActiveChange;
end; *)

constructor TDBLabel.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.FOnUpdateData := @DataUpdate;
  FDataLink.FOnDataChange := @DataChange;
  FDataLink.FOnActiveChange := @ActiveChange;
end;

destructor TDBLabel.Destroy;
begin
  FDataLink.Free;
  inherited; //Destroy;
end;

{ TFieldDataLink }

procedure TFieldDataLink.UpdateField;
begin
  FField := nil;
  if (((assigned(DataSource)) and (assigned(DataSource.DataSet))) and (FFieldName <> '')) then
    if (DataSource.DataSet.Active)
      then FField := DataSource.DataSet.FieldByName(FFieldName);
end;

procedure TFieldDataLink.ActiveChanged;
begin
  //inherited; //ActiveChanged;
  if (assigned(FOnActiveChange)) then
    FOnActiveChange(Self);
end;

procedure TFieldDataLink.RecordChanged(Field: TField);
begin
  //inherited; //RecordChanged(Field);
  if ((Field = nil) or (Field = FField)) then
    if (assigned(FOnDataChange)) then
      FOnDataChange(Self);
end;

procedure TFieldDataLink.UpdateData;
begin
  //inherited; //UpdateData;
  if (FModified) then
  begin
    if ((assigned(FField)) and (assigned(FOnUpdateData))) then
      FOnUpdateData(Self);
    FModified := false;
  end;
end;

procedure TFieldDataLink.Modified;
begin
  FModified := true;
end;

{ TConnection }

procedure TConnection.SetActive(AValue: boolean);
begin
  if ((FURI <> '') and AValue) then
  begin
    DoBeforeConnect();
    FActive := AValue;
    FReq := TJSXMLHttpRequest.new;
    FReq.addEventListener('load', @onLoad);
    FReq.open('GET',FURI,true);
    FReq.send();
  end;
end;

function TConnection.onLoad(Event: TEventListenerEvent): boolean;
var
  J: JSValue;
 JA: JSValue;

begin
  if (FReq.status = 200) then
  begin
    J := TJSJSON.parse(FReq.responseText);
    JA := TJSObject(J).Properties[FDataNode];
    DoAfterConnect();
    if (assigned(FDS)) then
    begin
      FDS.Rows := TJSArray(JA);
      FDS.Open();
    end else
    DoError(FReq.status);
  end;
end;

procedure TConnection.RegisterDataSet(value: TDataSet);
begin
  FDS := value;
end;

procedure TConnection.DoBeforeConnect;
begin
  if (assigned(FBeforeConnect)) then
    FBeforeConnect(Self);
end;

procedure TConnection.DoAfterConnect;
begin
  if (assigned(FAfterConnect)) then
    FAfterConnect(Self);
end;

procedure TConnection.DoError(ErrorCode: Integer);
begin
  if (assigned(FOnConnectError)) then
    FOnConnectError(Self, ErrorCode);
end;

constructor TConnection.Create(AOwner: TComponent);
begin
  inherited; //Create(AOwner);
  FDS := nil;
end;

{ TDataSet }
procedure TDataSet.SetConnection(Value: TConnection);
begin
  if (assigned(Value)) then
    Value.RegisterDataSet(Self);
end;

Function TDataSet.CreateFieldMapper : TJSONFieldMapper;
begin
  Result := TJSONObjectFieldMapper.Create;
end;

end.

