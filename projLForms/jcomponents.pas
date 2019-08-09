(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ Created by warleyalex                                                 ║
   ║ from Brazil                                                           ║   ╚═══════════════════════════════════════════════════════════════════════╝ *)

unit JComponents;

{$MODE objfpc}{$H+}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, Types, Math, JS, Web, uDom;

//type
//  JApplication = class external name 'pas["program"].Application'
//  public
//    procedure GotoForm(aFormName: string; speed: integer = 350; aCallBackFn:
//      TProcedure = nil);
 // end;

//var
 // Application: JApplication; external name 'pas["program"].Application';

type
  TSJHTMLTemplateElement = class external name 'HTMLTemplateElement' (TJSHTMLElement)
  public
    content: TJSDocumentFragment;
  end;

type
  TProcedureRef = procedure;
  TMouseClickEv = procedure(sender: TObject);
  TMouseClickEvent = procedure(sender: TObject) of object;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TControl                                                              ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  { TControl }
  TControl = class
  private
    { private declarations }
    FName: string;
    FTagId: string;
    FHandle: TJSHTMLElement;
    FParent: TControl;
  protected
    { protected declarations }
    procedure InitializeObject; virtual;
    procedure FinalizeObject; virtual;
    function createElementTagObj: TJSElement; virtual;
    procedure RegisterChild(aChild: TObject); virtual;
  public
    { public declarations }
    constructor Create(AOwner: TControl); reintroduce; virtual;
    destructor Destroy; override;
    { check if element already in DOM? Execute now :: Valid/ReadyExecute}
    function Valid: Boolean;
    procedure ReadyExecute(OnReady: TFrameRequestCallback);

    procedure BeginUpdate;
    procedure EndUpdate; virtual;
  published
    {  published properties }
    property Name: string read FName write FName;
    property Handle: TJSHTMLElement read FHandle write FHandle;
    //property Parent: TControl read FParent;
    // experimental
    property Parent: TControl read FParent write FParent;
    TabOrder: Integer;
  end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TMovableControl                                                       ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  { TMovableControl }
  TMovableControl = class(TControl)
  private
  protected
    {  protected properties }
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetTop: Integer; virtual;
    procedure SetLeft(const aValue: Integer); virtual;
    procedure SetTop(const aValue: Integer); virtual;
    procedure SetWidth(aValue: Integer); virtual;
    procedure SetHeight(aValue: Integer); virtual;
    procedure doLayout; overload;
    function setPropertyPosition(fTagRef: TJSCSSStyleDeclaration; position:
      string): Integer;
    procedure InitializeObject; override; //empty;
    procedure Resize; virtual;
    procedure FinalizeObject; override;
  public
    {  public declarations }
     (* set the value of a CSS property
        propertyName: specifies the name of the style property to remove.
        propertyValue: specifies the value for the property
     *)
    procedure setProperty(propertyName, propertyValue: string);
    procedure setAttribute(propertyName, propertyValue: string);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: Integer); overload;
      virtual;
  published
    {  published declarations }
    property Left: Integer read GetLeft write setLeft;
    property Top: Integer read GetTop write setTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomControl                                                        ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  { TCustomControl }
  TCustomControl = class(TMovableControl)
  private
    { private declarations }
    FOnClick: TMouseClickEvent;
    procedure _setMouseClick(const aValue: TMouseClickEvent);
  protected
    { protected declarations }
    procedure InitializeObject; override;
  public
    { public declarations }
    procedure CBClick(eventObj: TJSEvent); virtual;
  published
    {  published properties }
    property OnClick: TMouseClickEvent read FOnClick write _setMouseClick;
  end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomForm                                                           ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  { TCustomForm }
  TCustomForm = class(TCustomControl)
  private
    { private declarations }
    FFormIndex: Integer;
    FCaption: string;
    FInitialized: Boolean;
  protected
    { protected declarations }
    procedure LoadDFMValues; virtual;
  public
    { published declarations }
    constructor Create(AOwner: TControl); override;
    destructor Destroy; override;
    procedure FormActivated; virtual;
    procedure FormDeactivated; virtual;
    procedure InitializeForm; virtual;
    property FormIndex: Integer read FFormIndex write FFormIndex;
  published
    { published declarations }
    property Caption: string read FCaption write FCaption;
    // experimental
    ClientHeight: Integer;
    ClientWidth: Integer;
  end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TForm                                                                 ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  TWForm = class(TCustomForm)
  end;

  TFormClass = class of TWForm;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ JSwiper                                                               ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  JSwiper = class external name 'Swiper'
    constructor New(targetRef: string); overload;
    function _slideNext: JSwiper;  external name 'slideTo';
    function swipeNext: JSwiper; external name 'slideTo';
    function _slideTo(form: integer): JSwiper; external name 'slideTo';
    function swipeTo(form: integer): JSwiper; overload; external name 'slideTo';
    function swipeTo(form: integer; speed: Integer): JSwiper; overload; external name 'slideTo';
    function swipeTo(form: integer; speed: Integer; callbackFn: TProcedure):
      JSwiper; overload; external name 'slideTo';
    allowTouchMove: boolean;
  end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomApplication                                                    ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  { TCustomApplication }
  TCustomApplication = class(TControl)
  private
    FOldForm: Integer;
    FForm: TControl;
    //FSwiper: JSwiper; external name 'swiper';
    FormNames: array of string;
    FormsClasses: array of TFormClass;
    //see JForm: TFormClass = class of JW3Form;
    FormsInstances: array of TWForm;
  protected
    procedure ApplicationStarting; virtual;
  public
    Swiper: JSwiper;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RunApp;
    procedure CreateForm(FormName: string; aClassType: TFormClass);
    procedure GotoForm(aFormName: string; speed: integer = 350; aCallBackFn:
      TProcedure = nil);
  published
    //property swiper: JSwiper read FSwiper write FSwiper;
    property CurrentForm: Integer read FOldForm write FOldForm;
  end;

  { global methods }
    //function Application: TCustomApplication;
    function GenID: string;

var
  Application: TCustomApplication;

implementation

var
  __p: integer;

function GenID: string;
begin
  inc(__p);
  result := 'J' + IntToStr(__p);
end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TControl                                                              ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
function TControl.Valid: Boolean;
begin
  // result := not( variant(self = undefined) ?? variant(self = null) );

  if not ((Self = undefined) or (Self = null)) then
    result := true
  else
    result := false;

  //asm
  //  Result = (!((this==undefined)||(this==null)))?true:false;
  //end;

end;

procedure TControl.ReadyExecute(OnReady: TFrameRequestCallback);
begin
  (*
  if Self.Valid then
  begin
    { Element already in DOM? Execute now }
    if Self.Ready then
      Self.OnReady else
    { Try again ASAP }
    requestAnimationFrame(OnReady);
  end;
 *)
  if Self.Valid then
  begin
    { Element already in DOM? Execute now }
   // if Self.Ready then
    ///  OnReady
   // else
    { Try again ASAP }
    window.requestAnimationFrame(OnReady);
  end;
end;

procedure TControl.BeginUpdate;
begin
  // experimental
end;

procedure TControl.EndUpdate;
begin
  // experimental
end;

constructor TControl.Create(AOwner: TControl);

  function AsObject(const aValue: JSValue): JSValue;
  begin
    if (aValue <> undefined) and (aValue <> null) then
    begin
      result := aValue;
    end;
  end;
begin
  (* Keep parent *)
  FParent := TControl(AOwner);

  (* We have to call this here, otherwise FParent will be NIL
     when InitiateObject is called *)
  inherited Create;
  try
    FTagId := GenID; // generate 'OBJ' + number
    TJSElement(FHandle) := createElementTagObj;
  except
    on e: exception do
      console.log(e.Message);
  end;

  (* avoid affecting window->document->body *)
  if AsObject(FHandle) <> AsObject(window.document.body) then
  begin
    (* apply tag-id *)
    if Length(FTagId) > 0 then
      TJSElement(FHandle).setAttribute('id', FTagId);
  end;

  (* register with parent if valid *)
  if FParent <> nil then
    FParent.RegisterChild(Self);

  (* setup decendant instances *)
  InitializeObject;
end;

procedure TControl.RegisterChild(aChild: TObject);
begin
  Self.Handle.appendChild(TJSNode(TControl(aChild).Handle));
end;

destructor TControl.Destroy;
begin
  inherited Destroy;
end;

function TControl.createElementTagObj: TJSElement;
begin
  Result := document.createElement('div');
end;

procedure TControl.InitializeObject;
begin
  // empty  to be overriding
end;

procedure TControl.FinalizeObject;
begin
  // empty
end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TMovableControl                                                       ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
function TMovableControl.setPropertyPosition(fTagRef: TJSCSSStyleDeclaration;
  position: string): Integer;
begin
  FtagRef := document.defaultView.getComputedStyle(Self.Handle,
    TJSElement(null));

  if (assigned(FtagRef)) then
    result := parseInt(FtagRef.getPropertyValue(position));
end;

function TMovableControl.GetLeft: Integer;
begin
  result := setPropertyPosition(TJSCSSStyleDeclaration(Self.Handle), 'left');
  //  result := _(Self.Handle).offset().left;
end;

procedure TMovableControl.SetLeft(const aValue: Integer);
begin
  setProperty('left', IntToStr(aValue) + 'px');
  // TJSHTMLElement(Self.Handle).style['left'] := IntToStr(aValue) + 'px';
end;

function TMovableControl.GetTop: Integer;
begin
  result := setPropertyPosition(TJSCSSStyleDeclaration(Self.Handle), 'top');
  //result := _(Self.Handle).offset().top;
end;

procedure TMovableControl.SetTop(const aValue: Integer);
begin
  setProperty('top', IntToStr(aValue) + 'px');
end;

function TMovableControl.GetWidth: Integer;
begin
  //  if Handle then
  result := Math.Floor(Self.Handle.offsetWidth);
end;

procedure TMovableControl.SetWidth(aValue: Integer);
begin
  (*
    aValue := Max(aValue, 0);
    if aValue<>GetWidth then
    begin
      Handle.style['width'] := IntToStr(aValue) + 'px';
    end;
  *)
  setProperty('width', IntToStr(aValue) + 'px');
end;

function TMovableControl.GetHeight: Integer;
begin
  //  if Handle then
  Result := Math.Floor(Self.Handle.offsetHeight);
end;

procedure TMovableControl.SetHeight(aValue: Integer);
begin
  (*
    aValue := Max(aValue, 0);
    if aValue<>GetHeight then
    begin
      Handle.style['height'] := IntToStr(aValue) + 'px';
    end;
  *)
  setProperty('height', IntToStr(aValue) + 'px');
end;

procedure TMovableControl.SetBounds(aLeft, aTop, aWidth, aHeight: Integer);
var
  mSized: Boolean;
begin
  aWidth := Max(0, aWidth);
  aHeight := Max(0, aHeight);

  mSized := (aWidth <> Self.Handle.offsetWidth) or (aHeight <> Self.Handle.offsetHeight);
  setProperty('left', IntToStr(aLeft) + 'px');
  setProperty('top', IntToStr(aTop) + 'px');
  setProperty('width', IntToStr(aWidth) + 'px');
  setProperty('height', IntToStr(aHeight) + 'px');

  (*
  //_(Self.Handle).css('left', IntToStr(aLeft) + 'px');
  //_(Self.Handle).css('top', IntToStr(aTop) + 'px');
  //_(Self.Handle).css('width', IntToStr(aWidth) + 'px');
  //_(Self.Handle).css('height', IntToStr(aHeight) + 'px');


 mSized := (aWidth <> Self.Handle.offsetWidth) or (aHeight <> Self.Handle.offsetHeight);

 _(Self.Handle).css(
    new(['left',   IntToStr(aLeft) + 'px',
         'top',    IntToStr(aTop) + 'px',
         'width',  IntToStr(aWidth) + 'px',
         'height', IntToStr(aHeight) + 'px'
         ])
  );*)

end;

procedure TMovableControl.doLayout;
begin
  //if Handle then
  (*
    _(Self.Handle)
      .transform("translate3d("+_(Self.Handle).css("left")+", "+_(Self.Handle).css("top")+", 0px)")
      .css(
        CLASS
          "position" := "absolute";
          "width"    := IntToStr(Self.GetWidth)+'px';
          "height"   := IntToStr(Self.GetHeight)+'px';
          "top"      := "initial";
          "left"     := "initial";
        END);
  *)
end;

procedure TMovableControl.InitializeObject;
begin
  inherited;

end;

procedure TMovableControl.Resize;
begin

end;

procedure TMovableControl.FinalizeObject;
begin
  inherited FinalizeObject;
end;

procedure TMovableControl.setProperty(propertyName, propertyValue: string);
begin
  Self.Handle.style.setProperty(propertyName, propertyValue);
  //_(Self.Handle).css(propertyName, propertyValue);
end;

procedure TMovableControl.setAttribute(propertyName, propertyValue: string);
begin
  Self.Handle.setAttribute(propertyName, propertyValue);
  //_(Self.Handle).attr(propertyName, propertyValue);
end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomControl                                                        ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
procedure TCustomControl.InitializeObject;
begin
  inherited InitializeObject;
  Self.Handle.addEventListener('click', @CBClick);
//  _(Self.Handle).on('click', @CBClick);
//  _(Self.Handle).click(@CBClick);
end;

procedure TCustomControl.CBClick(eventObj: TJSEvent);
begin
  eventObj.stopPropagation;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCustomControl._setMouseClick(const aValue: TMouseClickEvent);
begin
  FOnClick := aValue;
end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomForm                                                           ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
constructor TCustomForm.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  Self.Handle.classList.add('swiper-slide', 'TCustomForm');
  //Self.Handle.style['position'] := 'fixed';
    LoadDFMValues;
end;

destructor TCustomForm.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomForm.FormActivated;
begin
  if not FInitialized then
  begin
    FInitialized := true;
    InitializeForm;
    //LayoutChildren;
    //Self.Resize; //--> warleyalex
  end
end;

procedure TCustomForm.FormDeactivated;
begin
  // empty
end;

procedure TCustomForm.InitializeForm;
begin
  // empty
end;

procedure TCustomForm.LoadDFMValues;
begin
  // empty
end;

(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ TCustomApplication                                                    ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
constructor TCustomApplication.Create;
var
  docFragment: TJSDocumentFragment;
  div_, div_0: TJSElement;
begin
  docFragment := document.createDocumentFragment();
  // contains all gathered nodes
  div_ := document.createElement('DIV');
  div_.setAttribute('class', 'swiper-container');
  div_.setAttribute('style', 'width:100%; height:' +
    IntToStr(window.screen.height) + 'px;');
  //div_.setAttribute("style", 'width:100%; height:'+window.screen.height+'px;'+ ' position:relative;');
  docFragment.appendChild(div_);

  div_0 := document.createElement('DIV');
  div_0.setAttribute('class', 'swiper-wrapper');
  div_0.setAttribute('style', 'width:100%; height:100%;');
  div_.appendChild(div_0);
  document.body.appendChild(docFragment);

  inherited Create;

  (* set class instance variable to Display *)
//  if not Assigned(FForm) then
    FForm := TControl.Create(nil);
  TJSElement(FForm.Handle) :=
    document.querySelector('.swiper-container').firstElementChild;

end;

destructor TCustomApplication.Destroy;
begin
  inherited;
end;

procedure TCustomApplication.RunApp;
begin
  ApplicationStarting;
  (* swiper enabled after forms created... *)

  if not Assigned(Swiper) then
    Swiper := JSwiper.New('.swiper-container');
    Swiper.allowTouchMove:=false;
end;

procedure TCustomApplication.CreateForm(FormName: string;
  aClassType: TFormClass);
var
  k: integer;
begin
  TJSArray(FormNames).push(FormName);
  TJSArray(FormsClasses).push(aClassType);
  TJSArray(FormsInstances).push(nil);

  for k := 0 to TJSArray(FormNames).Length - 1 do
  begin
    if FormsInstances[k] = nil then
    begin
      FormsInstances[k] := FormsClasses[k].Create(FForm);
      // create the form instances
      FormsInstances[k].FormIndex := k; // set Form Index
      FormsInstances[k].Name := formName;
      FormsInstances[k].FormActivated;
      // invoke initializeForm method --> invoke FormActivated method
    //(FormsInstances[k]).InitializeObject;  // --> why this fuck one is doing here
    end;
  end;

end;

procedure TCustomApplication.GotoForm(aFormName: string; speed: integer;
  aCallBackFn: TProcedure);
var
  i: integer;

  (* ╔═════════════════════════════════════════════════════════════╗
     ║ pass the FormName parameter and return the FormIndex        ║
     ╚═════════════════════════════════════════════════════════════╝ *)
  function IndexOfFormName(aformName: string): Integer;
  var
    lcName: string;
    i: Integer;
  begin
    lcName := Trim(LowerCase(aformName));
    for i := 0 to TJSArray(FormNames).Length - 1 do
      if LowerCase(FormsInstances[i].Name) = lcName then
        exit(FormsInstances[i].FormIndex);
  end;

  (* ╔═════════════════════════════════════════════════════════════╗
     ║ pass the FormIndex parameter and return the form instance   ║
     ╚═════════════════════════════════════════════════════════════╝ *)
  function getForm(index: Integer): TWForm;
  begin
    Result := TWForm(FormsInstances[index]);
  end;

var
  n: string;
begin
  (* ╔══════════════════════════════════════════════════════════╗
     ║ Invoke FormDeactivate event before SwipeTo/slideTo page  ║
     ╚══════════════════════════════════════════════════════════╝ *)
  for i := 0 to TJSArray(FormNames).Length - 1 do
  begin
    if Trim(LowerCase(FormsInstances[i].Name)) =
      Trim(LowerCase(getForm(FOldForm).Name)) then
    begin
      FormsInstances[i].FormDeactivated; // ---> invoke FormDeactivated
    end;
  end;

  if assigned(Swiper) then
      Swiper.swipeTo(IndexOfFormName(aFormName), speed, aCallBackFn);

  (* ╔══════════════════════════════════════════════════════════╗
     ║ Invoke FormActivate event after SwipeTo/slideTo page     ║
     ╚══════════════════════════════════════════════════════════╝ *)

  n := Trim(LowerCase(aFormName));
  for i := 0 to TJSArray(FormNames).Length - 1 do
  begin
    if Trim(LowerCase(FormsInstances[i].Name)) = Trim(LowerCase(aFormName)) then
    begin
      FormsInstances[i].FormActivated; // ---> invoke FormActivated
      FOldForm := FormsInstances[i].FormIndex;
    end;
  end;

end;

procedure TCustomApplication.ApplicationStarting;
begin
  // empty
end;

end.

