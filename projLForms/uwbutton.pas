unit uWButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS, Web,
  JComponents;
 // pas2js.Element;

(*type
TButton = class(TCustomControl)
public
  constructor Create(parent: TCustomControl); virtual;
end; *)

type

  { TButton }
  TButton = class(TCustomControl)
  private
    //FID: String;
    //FStyleClass: String;
    FLeft: String;// = '0px';
    FTop: String;// = '0px';
    FWidth: String;// = '100%';
    FHeight: String;// = '100%';
    FEnabled : boolean;
    FCaption: String;
    //FOnObjectReady: TNotifyEvent;
  protected
    function GetCaption: String;
    procedure SetCaption(Value : String);
    procedure InitializeObject; override;
    //procedure StyleTagObject; override; empty; (* comment this if you need style attribute in the parent *)
    //procedure ObjectReady; override;
    function createElementTagObj: TJSElement; override;
  public
    constructor Create(AOwner: TControl); override;
  published
   // property ID: String read FID write FID; //setID;
   // property StyleClass: String read FStyleClass write FStyleClass;
    property _Left: String read FLeft write FLeft;
    property _Top: String read FTop write FTop;
    property _Width: String read FWidth write FWidth;
    property _Height: String read FHeight write FHeight;
    property CustomSize: boolean read FEnabled write FEnabled;
    property Caption : String read GetCaption write SetCaption;
    //property OnObjectReady: TNotifyEvent read FOnObjectReady write FOnObjectReady;
end;

implementation

(*
{ TWButton }
constructor TButton.Create(parent: TCustomControl);
begin
//  inherited Create('button', parent);
  inherited Create(parent);

  TJSHTMLElement(Self.Handle).classList.add('button');
  //SetProperty('color','white');
  //SetProperty('border-radius', '4px');
  //SetProperty('background', '#699BCE');
  //SetProperty('cursor','pointer');
  //SetProperty('box-shadow','0 -1px 1px 0 rgba(0, 0, 0, 0.25) inset, 0 1px 1px 0 rgba(0, 0, 0, 0.10) inset;)');

end;*)

{ ╔════════════════════════════════════════════╗
  ║ TWButton                                   ║
  ╚════════════════════════════════════════════╝ }
constructor TButton.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
//  document.createElement('button');
end;

function TButton.createElementTagObj: TJSElement;
begin
  Result := document.createElement('button');
end;

function TButton.GetCaption: String;
begin
  //if (Handle) then
    //Result := Handle.innerHTML;
    //Result := SMS(Self.Handle).text();
  Result := Self.Handle.innerText;
end;

procedure TButton.SetCaption(Value: String);
begin
  //if (Handle) then
    //SMS(Self.Handle).text(Value);
    //Handle.innerHTML := Value;
    Self.Handle.innerText:= Value;
end;

procedure TButton.InitializeObject;
  procedure ReadyAndExecute(aTime: TJSDOMHighResTimeStamp);
  begin

  (*
  SMS(Self.Handle).css('text-align', 'center');

  SMS(Self.Handle)
    //.transform("translate3d("+FLeft??String(SMS(Self.Handle).css("left"))+", "+FTop??String(SMS(Self.Handle).css("top"))+", 0px)")
    .css(
      CLASS
        "position":= "absolute";
        "width"    := IntToStr(Self.Width) + "px";
        "height"   := IntToStr(Self.Height) + "px";
        "top"      := SMS(Self.Handle).css("top");
        "left"     := SMS(Self.Handle).css("left");
      END);

 //doLayout;

  *)
  //console.log('onReady was called');

  end;

begin
  inherited; //GenerateUniqueObjectId
   TJSElement(Self.Handle).classList.add('button');
  // TJSElement(Self.Handle).ReadyExecute(@ReadyAndExecute);
  //ReadyExecute(@ReadyAndExecute);

 end;

end.

