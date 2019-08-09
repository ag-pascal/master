(*
 ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════╗
 ║ This is a custom wrapper for DOM manipulation. This library utilizes most edge and high-performance   ║
 ║ methods for DOM manipulation. You don't need to learn something new, its usage is very simple because ║
 ║ it has the same syntax as well known jQuery library with support of the most popular and widely used  ║
 ║ methods and jQuery-like chaining.                                                                     ║
 ║ by warleyalex                                                                                         ║
 ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════╝   }
 *)

unit uDOM;

{$MODE objfpc}
{$MODESWITCH externalclass}

interface

uses
  Types, Web, JS, Classes, SysUtils {, libjquery};

type
  JDom7TopLeft = class external name 'Dom7TopLeft'
    top, left: integer;
  end;

type
  JDom7 = class;
  //TCallback = Procedure (args : JSValue);
  //TCallbackEvent = Procedure (args : JSValue);

(*
type
  TCallbacks = class external name 'Callbacks'
  Public
    function add(aCallBack : TCallBack) : TCallbacks;
    function add(aCallBack : Array of TCallBack) : TCallbacks;
    function add(aCallBack : TCallBackEvent) : TCallbacks;
    function add(aCallBack : Array of TCallBackEvent) : TCallbacks;
    function disable : TCallBacks;
    function disabled : Boolean;
    function empty : TCallBacks;
    function fire(arguments : JSValue) : TCallbacks; varargs;
    function fired : Boolean;
    function fireWith(context : JSValue; arguments : JSValue) : TCallbacks;
    function has(aCallBack : TCallBack) : Boolean;
    function has(aCallBack : TCallBackEvent) : Boolean;
    function lock : TCallBacks;
    function locked : boolean;
    function remove(aCallBack : TCallBack) : TCallbacks;
    function remove(aCallBack : Array of TCallBack) : TCallbacks;
    function remove(aCallBack : TCallBackEvent) : TCallbacks;
    function remove(aCallBack : Array of TCallBackEvent) : TCallbacks;
  end;*)

  { JDom7 }

  JDom7AddClassHandler = reference to function(aIndex: Integer; AClass: string):
    string;
  JDom7AttrHandler = reference to function(aIndex: Integer; aAttr: string):
    JSValue;
  JDom7CSSHandler = reference to function(aIndex: Integer; AClass: string):
    JSValue;
  JDom7EachHandler = reference to function(aIndex: Integer; AElement:
    TJSElement): Boolean;
  JDom7FilterHandler = reference to function(aIndex: Integer; AElement:
    TJSElement): Boolean;
  JDom7HeightHandler = reference to function(aIndex: Integer; AHeight: jsValue):
    JSValue;
  JDom7HTMLHandler = reference to function(aIndex: Integer; aHTML: string):
    string;
  //JDom7MapHandler = Reference to Function (aIndex : Integer; AElement : TJSElement) : TJSObject;
  JDom7OffsetHandler = reference to function(aIndex: Integer; aCoords:
    JDom7TopLeft): JDom7TopLeft;
  JDom7PropHandler = reference to function(aIndex: Integer; oldProp: JSValue):
    JSValue;
  //JDom7QueueHandler = Reference to procedure;
  JDom7TextHandler = reference to function(aIndex: Integer; aString: string):
    string;
  JDom7ToggleClassHandler = reference to function(aIndex: Integer; aClassName:
    string; AState: Boolean): string;
  JDom7ValHandler = reference to function(aIndex: integer; aValue: string):
    string;
  JDom7WidthHandler = reference to function(aIndex: Integer; AHeight: jsValue):
    JSValue;

  JDom7DeQueueFunction = reference to procedure;
  //JDom7AddQueueHandler = Reference to Procedure (aFunc : JDom7DeQueueFunction);

  //TAjaxEvent = class external name 'AjaxEvent' (TJSEvent);

  TDeferredDoneHandler = reference to function: Boolean;

  JDom7Deferred = class external name 'Deferred'(TJSObject)
  public
    function done(aHandler: TDeferredDoneHandler): JDom7Deferred; overload;
    function done(aHandlers: array of TDeferredDoneHandler): JDom7Deferred;
      overload;
  end;

  TJQXHR = class;

  TJQXHRDoneHandler = reference to function(aData: jsValue; textStatus: string;
    aJQXHR: TJQXHR): boolean;
  TJQXHRFailHandler = reference to function(aJQXHR: TJQXHR; textStatus: string;
    AErrorThrown: jsValue): boolean;
  TJQXHRAlwaysHandler = reference to function(arg1: TJSObject; textStatus:
    string; arg2: TJSObject): boolean;

  TJQXHR = class external name 'jqXHR'(JDom7Deferred)
  private
    FReadyState: NativeInt; external name 'readyState';
    //FResponse: JSValue; external name 'response';
    FResponseText: string; external name 'responseText';
    FresponseXML: TJSDocument; external name 'responseXML';
    //FUpload: TJSXMLHttpRequestUpload; external name 'upload';
    FStatus: NativeInt; external name 'status';
    FStatusText: string; external name 'statustext';
  public
    function getResponseHeader(aName: string): string;
    function getAllResponseHeaders: string;
    procedure overrideMimeType(aType: string);
    procedure setRequestHeader(aName, AValue: string);
    procedure done(aHandler: TJQXHRDoneHandler); overload;
    procedure always(aHandler: TJQXHRAlwaysHandler); overload;
    procedure fail(aHandler: TJQXHRFailHandler); overload;
    procedure _then(aSuccess: TJQXHRDoneHandler; aFail: TJQXHRFailHandler);
      overload;
    procedure abort;
    procedure abort(AStatusText: string);
    property readyState: NativeInt read FReadyState;
    property ResponseHeaders[aName: string]: string read getResponseHeader;
    property responseXML: TJSDocument read FresponseXML;
    property responseText: string read FResponseText;
    property status: NativeInt read FStatus;
    property statusText: string read FStatusText;
  end;

  TJSAjaxSettings = class;

  JDom7AjaxSettingsHandler = reference to function(aHXR: TJQXHR; aOptions:
    TJSAjaxSettings): Boolean;
  JDom7AjaxSettingsDataFilter = reference to function(aData: string; aType:
    string): JSValue;
  JDom7AjaxSettingsErrorHandler = reference to function(aHXR: TJQXHR; aOptions:
    TJSAjaxSettings; aStatus, aError: string): Boolean;
  JDom7AjaxSettingsSuccessHandler = reference to function(data: JSValue;
    aStatus: string; aHXR: TJQXHR): Boolean;
  JDom7AjaxSettsingsXHRHandler = reference to function: JSValue;

  TJSAjaxSettings = class external name 'Object'(TJSObject)
    accepts: TJSObject;
    async: boolean;
    beforeSend: JDom7AjaxSettingsHandler;
    cache: boolean;
    complete: JDom7AjaxSettingsHandler;
    contents: TJSObject;
    contentType: string;
    context: TJSObject;
    converters: TJSObject;
    crossDomain: boolean;
    data: JSValue;
    dataFilter: JDom7AjaxSettingsDataFilter;
    dataType: string;
    error: JDom7AjaxSettingsErrorHandler;
    global: boolean;
    headers: TJSObject;
    ifModified: Boolean;
    isLocal: Boolean;
    json: string;
    jsonpCallback: string;
    method: string;
    mimeType: string;
    password: string;
    processData: Boolean;
    scriptCharset: string;
    statusCode: TJSObject;
    success: JDom7AjaxSettingsSuccessHandler;
    timeout: NativeInt;
    traditional: boolean;
    url: string;
    username: string;
    xhr: JDom7AjaxSettsingsXHRHandler;
    xhrFields: TJSObject;
  end;

  JDom7AjaxTransportCompleteHandler = function(aStatus: NativeInt; aStatusText:
    string; responses, Headers: TJSObject): Boolean;
  JDom7AjaxTransportSendHandler = reference to function(headers: TJSObject;
    onComplete: JDom7AjaxTransportCompleteHandler): boolean;
  JDom7AjaxTransportAbortHandler = reference to function(): Boolean;
  JDom7AjaxTransport = record
    send: JDom7AjaxTransportSendHandler;
    abort: JDom7AjaxTransportAbortHandler;
  end;

  //JDom7AjaxTransportHandler = reference to Function (aOptions,aOriginalOptions : TJSObject; aXHR : TJQXHR) : JDom7AjaxTransport;
  //JDom7AjaxPrefilterHandler = reference to procedure (aOptions,aOriginalOptions : TJSObject; aXHR : TJQXHR);
  //JDom7AjaxEventHandler = Reference to Function (aEvent : TAjaxEvent; aHXR : TJQXHR; aOptions : TJSAjaxSettings) : Boolean;
  //JDom7AjaxErrorHandler = Reference to Function (aEvent : TAjaxEvent; aHXR : TJQXHR; aOptions : TJSAjaxSettings; aError : String) : Boolean;
  JDom7AjaxSuccessHandler = reference to function(aData: TJSObject; aStatus:
    string; aXHR: TJQXHR): Boolean;
  //JDom7AjaxLoadHandler = Reference to function (aResponseText,aStatus : String; aXHR : TJQXHR) : Boolean;
  //JDom7AjaxScriptHandler = Reference to function (aScript,aStatus : String; aXHR : TJQXHR) : Boolean;
  //JDom7AjaxHandler = Reference to procedure;

  TProcedure = procedure of object;
  JEvent = class external name 'Event'(TJSEvent);
  JBaseEventObject = class external name 'Object'(JEvent);
  JEventObject = class external name 'Object'(JBaseEventObject);
  JFunction_on_handler = function(eventObject: JEventObject): JSValue of object;
  JProcEvent = procedure(event: TJSEvent {JEvent}) of object;
  JProcEventJS = procedure(event: JSValue);
  eachCallBack = procedure(index, value: JSValue);
  eachCallBackProc = procedure(index: Double; element: JSValue);
  JDomFilterHandlerFunc = function(index: Double; element: JSValue): Boolean;

  { ╔════════════════════════════════════════════════╗
    ║ DOM7 Library                                   ║
    ╚════════════════════════════════════════════════╝ }

  JDom7 = class external name 'window.Dom7'(TJSObject)
  private
    FCSSHooks: TJSObject; external name 'cssHooks';
    FCSSNumber: TJSObject; external name 'cssNumber';
    FReady: TJSPromise; external name 'ready';
    function getEl(aIndex: Integer): TJSElement; external name 'get';
  public
    //-------- Classes --------//
        //function item(aIndex : NativeInt) : TJSNode; //TJSHTMLElement;
        //Property Nodes [aIndex : NativeInt] : TJSNode Read item; default;
    function addClass(const aClass: string): JDom7; overload;
    function addClass(const aClassFunction: JDom7AddClassHandler): JDom7;
      overload;
    function removeClass(const aClass: string): JDom7; overload;
    function removeClass(const aClassFunction: JDom7AddClassHandler): JDom7;
      overload;
    function hasClass(const aClassName: string): Boolean;
    function toggleClass(const aClass: string): JDom7; overload;
    function toggleClass(const aClass: string; aState: Boolean): JDom7;
      overload;
    function toggleClass(const aHandler: JDom7ToggleClassHandler): JDom7;
      overload;
    function toggleClass(const aHandler: JDom7ToggleClassHandler; AState:
      Boolean): JDom7; overload;

    //-------- Attributes and properties --------//
    function prop(const aPropertyName: string): JDom7; {JSValue;}
    overload;
    function prop(const aPropertyName: string; AValue: JSValue): JDom7;
      overload;
    function prop(const aPropertyName: string; propValue: boolean): JDom7;
      overload;
    function prop(const aPropertyName: string; propValue: string): JDom7;
      overload;
    function prop(const TJSObject): JDom7; overload;
    function prop(const propertiesObject: JSValue): JDom7; overload;
    function prop(const aPropertyName: string; aHandler: JDom7PropHandler):
      JDom7; overload;
    { function attr(Const attributeName : string) : JDom7; overload; }
    function attr(const attributeName: string): string; overload;
    function attr(const attributeName: string; const Value: string): JDom7;
      overload;
    function attr(const attributes: TJSObject): JDom7; overload;
    function attr(const attributes: JSValue): JDom7; overload;
    function attr(const attributeName: string; aHandler: JDom7AttrHandler):
      JDom7; overload;
    function removeAttr(const attributeName: string): JDom7;
    function val: JSValue; overload;
    {function val : JDom7; overload;}
    function val(const aValue: string): JDom7; overload;
    function val(newValue: JSValue): JDom7; overload;
    function val(const aValue: Integer): JDom7; overload;
    function val(const aValue: array of string): JDom7; overload;
    function val(aHandler: JDom7ValHandler): JDom7; overload;

    //-------- Data storage --------//
    class function data(aElement: TJSElement; const aKey: string; aValue:
      jsValue): TJSObject; overload;
    class function data(aElement: TJSElement; const aKey: string): TJSObject;
      overload;
    class function data(aElement: TJSElement): TJSObject; overload;
    function data(aKey: string; aValue: JSValue): JDom7; overload;
    function data(aObj: TJSObject): JDom7; overload;
    function data(aKey: string): TJSObject; overload;
    function data: TJSObject; overload;
    class function removeData(aElement: TJSElement; const aName: string): JDom7;
      overload;
    class function removeData(aElement: TJSElement): JDom7; overload;
    function removeData(const aName: string): JDom7; overload;
    function removeData(const aNames: array of string): JDom7; overload;
    function removeData: JDom7; overload;

    //-------- Data Set --------//
    function dataset(): JDom7; overload;

    //-------- CSS transform, transitions --------//
    function transform(CSSTransformString: string): JDom7; overload;
    function transition(transitionDuration: double): JDom7; overload;

    //-------- Events --------//
    function on(eventName: string; handler: JProcEventJS): JDom7; overload;
function on(eventName: string; handler: JProcEvent): JDom7; overload;
function on(eventName: string; handler: JProcEvent; useCapture: Boolean): JDom7;
  overload;
function on(eventName: string; delegatedTarget: string; handler: JProcEvent):
  JDom7; overload;
function on(eventName: string; delegatedTarget: string; handler: JProcEvent;
  useCapture: Boolean): JDom7; overload;

function on(objParams: TJSObject): JDom7; overload;
function on(eventName: string; handler: JFunction_on_handler = nil): JDom7;
  overload;
{function on(const eventName: JSValue; delegatedTarget: JSValue = undefined; handler: TFunction_on_handler = nil): JDom7; overload;}
// function on(const eventName: JSValue; delegatedTarget: JSValue; const handler: JFunction_on_handler = nil): JDom7; overload;
function on(const eventName: JSValue; const delegatedTarget: JSValue; const
  handler: JFunction_on_handler = nil): JDom7; overload;

function once(eventName: string; handler: JProcEvent): JDom7; overload;
function once(eventName: string; handler: JProcEvent; useCapture: Boolean):
  JDom7; overload;
function once(eventName: string; delegatedTarget: string; handler: JProcEvent):
  JDom7; overload;
function once(eventName: string; delegatedTarget: string; handler: JProcEvent;
  useCapture: Boolean): JDom7; overload;

function once(eventName: string; handler: JFunction_on_handler = nil): JDom7;
  overload;
{function once(eventName: JSValue; delegatedTarget: JSValue = undefined; handler: JFunction_on_handler = nil): JDom7; overload;}
function once(eventName: JSValue; const delegatedTarget: JSValue; handler:
  JFunction_on_handler = nil): JDom7; overload;
function off(eventName: string; handler: JProcEvent): JDom7; overload;
function off(eventName: string; handler: JProcEvent; useCapture: Boolean):
  JDom7; overload;
function off(eventName: string; delegatedTarget: string; handler: JProcEvent):
  JDom7; overload;
function off(eventName: string; delegatedTarget: string; handler: JProcEvent;
  useCapture: Boolean): JDom7; overload;

function off(eventName: string): JDom7; overload;
function off(eventName: string; handler: TProcedure): JDom7; overload;
{function off(eventName: JSValue; delegatedTarget: JSValue = undefined; handler: TProcedure = nil): JDom7; overload;}
function &off(eventName: string; delegatedTarget: JSValue; handler: TProcedure =
  nil): JDom7; overload;

function trigger(eventName: string; eventData: JSValue): JDom7; overload;

function transitionEnd(callback: TProcedure; permanent: Boolean): JDom7;
  overload;
function transitionEnd(callback: TProcedure): JDom7; overload;
function animationEnd(callback: TProcedure): JDom7; overload;

//-------- Styles --------//
function Width: Integer; overload;
function Width(aValue: Integer): JDom7; overload;
function Width(aValue: string): JDom7; overload;
function width(value: JSValue {String or Float}): JDom7; overload;
function Width(aHandler: JDom7WidthHandler): JDom7; overload;

function height: Integer;
function height(aValue: Integer): JDom7;
function height(aValue: string): JDom7;
function height(aValue: JSValue): JDom7;
function height(aHandler: JDom7HeightHandler): JDom7;
function outerHeight(IncludeMargin: Boolean): Integer; overload;
function outerHeight: Integer; overload;
function outerHeight(aValue: Integer): JDom7; overload;
function outerHeight(aValue: string): JDom7; overload;
function outerHeight(aValue: JSValue): JDom7; overload;
function outerHeight(aHandler: JDom7HeightHandler): JDom7; overload;
function outerWidth(IncludeMargin: Boolean): Integer; overload;
function outerWidth: Integer; overload;
function outerWidth(aValue: Integer): JDom7; overload;
function outerWidth(aValue: string): JDom7; overload;
function outerWidth(aHandler: JDom7WidthHandler): JDom7; overload;
//function offset: Double; overload;
function offset: JDom7TopLeft; overload;
function offset(const aOffset: JSValue{String or Float}): JDom7; overload;
function offset(const aOffset: JDom7TopLeft): JDom7; overload;
function offset(aHandler: JDom7OffsetHandler): JDom7; overload;
function offsetParent: JDom7;
function hide(): JDom7; overload;
function show(): JDom7; overload;
function css(const aPropertyName: TJSObject): string; overload;
function css(const aPropertyName: string): string; overload;
function css(const aPropertyNames: array of string): string; overload;
function css(const aPropertyName, Avalue: string): JDom7; overload;
function css(const aPropertyName: string; Avalue: Integer): JDom7; overload;
function css(const aPropertyName: string; AHandler: JDom7CSSHandler): JDom7;
  overload;
{function css(Const aProperty: String): JSValue {String or Float}; overload;
}
  function css(const aProperty: string; value: JSValue {String or Float}):
    JDom7; overload;
function css(const aPropertiesObject: JSValue): JDom7; overload;

//-------- Scroll --------//
function scrollLeft: Integer; overload;
function scrollLeft(aValue: Integer): JDom7; overload;
function scrollLeft(position: Integer; duration: Double): JDom7; overload;
function scrollLeft(position: Integer; duration: Double; callback: TProcedure):
  JDom7; overload;
function scrollLeft(position: integer; duration: integer; callback: TProcedure):
  JDom7; overload;
function scrollTop: Integer; overload;
function scrollTop(aValue: Integer): JDom7; overload;
function scrollTop(position: Double; duration: Double): JDom7; overload;
function scrollTop(position: Double; duration: Double; callback: TProcedure):
  JDom7; overload;
function scrollTop(position: integer; duration: integer; callback: TProcedure):
  JDom7; overload;
function scrollTo(left: Double; top: Double): JDom7; overload;
function scrollTo(left: Double; top: Double; duration: Double): JDom7; overload;
function scrollTo(left: Double; top: Double; duration: Double; callback:
  TProcedure): JDom7; overload;
function scrollTo(left, top, duration: integer; callback: TProcedure): JDom7;
  overload;

//-------- Dom Manipulation --------//
function add(const aSelector: string): JDom7; overload;
function add(const aSelector: string; AContext: TJSElement): JDom7; overload;
function add(const aElement: TJSElement): JDom7; overload;
function add(const aElement: array of TJSElement): JDom7; overload;
function add(const aQuery: JDom7): JDom7; overload;
function add(const aElement: JSValue): JDom7; overload;
function add(const aElement: TJSArray): JDom7; overload;
function each(aHandler: JDom7EachHandler): JDom7;
function each(obj: JSValue; callback: eachCallBack): JDom7; overload;
function each(callback: TProcedure): JDom7; overload;
function each(callback: eachCallBackProc): JDom7; overload;
function html: string; overload;
function html(const aHTML: string): JDom7; overload;
function html(const aHTML: JSValue): JDom7; overload;
function html(const aHandler: JDom7HTMLHandler): JDom7; overload;
function text: string; overload;
function text(const aText: string): JDom7; overload;
function text(const aText: Integer): JDom7; overload;
function text(const aText: Double): JDom7; overload;
function text(const aText: Boolean): JDom7; overload;
function text(aHandler: JDom7TextHandler): JDom7; overload;
function text(newTextContent: JSValue): JDom7; overload;
function &is(const aSelector: string): JDom7; external name 'is';
function &is(const aQuery: JDom7): JDom7; external name 'is';
function &is(aHandler: JDom7FilterHandler): JDom7; external name 'is';
function &is(const aElement: TJSElement): JDom7; external name 'is';
function &is(const aElements: array of TJSElement): JDom7; external name 'is';
function &is(CSSSelector: JSValue): JDom7; overload; external name 'is';
(* Dom7.isArray(obj) - Determine whether the argument is an array
   obj - object - Object to test whether or not it is an array
   returns a Boolean indicating whether the object is a JavaScript array *)
{ function isArray(obj: JSValue): JDom7; overload; }
function isArray(target: JSValue): Boolean; overload;
function index: Boolean;
(* Dom7.dataset(el) - Get element's data set (set of data- attributes) as plain Object
   el - HTMLElement or string (with CSS selector) - element with data- attributes to get dataset from
   returns a new plain object with dataset  *)
//function dataset(el: variant): JDom7; overload
function dataset(target: string {String or JHTMLElement or JDom7}): JSValue;
  overload;
function dataset(target: TJSElement {String or JHTMLElement or JDom7}): JDom7;
  overload;
function dataset(target: JSValue {String or JHTMLElement or JDom7}): JDom7;
  overload;
function eq(AIndex: Integer): JDom7; overload;
function eq(index: JSValue): JDom7; overload;
function eq(index: Double): Boolean; overload;
function append(HTMLString: string): JDom7; overload;
function append(HTMLElement: JSValue): JDom7; overload;
function appendTo(element: string {String or JElement or JDom7}): JDom7;
function appendTo(element: TJSElement {String or JElement or JDom7}): JDom7;
(* Dom7.parseUrlQuery(url) - parse url query get parameters
   url - string - url with GET parameters. Required.
   Method returns object with query parameters *)
function parseUrlQuery(url: string): JDom7; overload;
function prepend(HTMLString: string): JDom7; overload;
function prepend(HTMLElement: JSValue): JDom7; overload;
function prependTo(element: string {String or JElement or JDom7}): JDom7;
function prependTo(element: TJSElement {String or JElement or JDom7}): JDom7;
function prependTo(element: JSValue {String or JElement or JDom7}): JDom7;
function insertBefore(element: JSValue {String or JElement or JDom7}): JDom7;
function insertBefore(element: TJSElement {String or JElement or JDom7}): JDom7;
function insertBefore(element: string {String or JElement or JDom7}): JDom7;
function insertAfter(element: JSValue {String or JElement or JDom7}): JDom7;
function insertAfter(element: TJSElement {String or JElement or JDom7}): JDom7;
function insertAfter(element: string {String or JElement or JDom7}): JDom7;
function next: JDom7; overload;
function next(const aSelector: string): JDom7; overload;
function nextAll: JDom7; overload;
function nextAll(const aSelector: string): JDom7; overload;
function nextAll(const aSelector: JSValue): JDom7; overload;
function nextUntil: JDom7; overload;
function nextUntil(const aSelector: string): JDom7; overload;
function nextUntil(const aSelector, aFilter: string): JDom7; overload;
function nextUntil(const aElement: TJSElement): JDom7; overload;
function nextUntil(const aElement: TJSElement; aFilter: string): JDom7;
  overload;
function nextUntil(const aQuery: JDom7): JDom7; overload;
function nextUntil(const aQuery: JDom7; aFilter: string): JDom7; overload;
function prev: JDom7; overload;
function prev(const aSelector: string): JDom7; overload;
function prev(const aSelector: JSValue): JDom7; overload;
function prevAll: JDom7; overload;
function prevAll(const aSelector: string): JDom7; overload;
function prevAll(const aSelector: JSValue): JDom7; overload;
(*function prevUntil : JDom7; overload;
function prevUntil(const aSelector : String) : JDom7; overload;
function prevUntil(const aSelector,aFilter : String) : JDom7; overload;
function prevUntil(const aElement : TJSElement) : JDom7; overload;
function prevUntil(const aElement : TJSElement; aFilter : String) : JDom7; overload;
function prevUntil(const aQuery : JDom7) : JDom7; overload;
function prevUntil(const aQuery : JDom7; aFilter : String) : JDom7; overload;*)
function parent: JDom7;
function parent(const ASelector: string): JDom7;
function parent(const ASelector: JSValue): JDom7;
function parents: JDom7;
function parents(const ASelector: string): JDom7;
function parents(const ASelector: JSValue): JDom7;
(*function parentsUntil : JDom7;
function parentsUntil(const aSelector : String) : JDom7;
function parentsUntil(const aSelector,aFilter : String) : JDom7;
function parentsUntil(const aElement : TJSElement) : JDom7;
function parentsUntil(const aElement : TJSElement; aFilter : String) : JDom7;
function parentsUntil(const aQuery : JDom7) : JDom7;
function parentsUntil(const aQuery : JDom7; aFilter : String) : JDom7; *)
function find(const aSelector: string): JDom7; overload;
function find(const aSelector: JSValue): JDom7; overload;
function find(const aQuery: JDom7): JDom7; overload;
function find(const aElement: TJSElement): JDom7; overload;
function children(selector: JSValue): JDom7; overload;
function children(const aSelector: string): JDom7; overload;
function children: JDom7; overload;
function filter(callback: TProcedure): JDom7; overload;
function filter(callback: JDomFilterHandlerFunc): JDom7; overload;
function filter(const aSelector: string): JDom7; overload;
function filter(aHandler: JDom7FilterHandler): JDom7; overload;
function filter(const aQuery: JDom7): JDom7; overload;
function filter(const aElement: TJSElement): JDom7; overload;
function filter(const aElements: array of TJSElement): JDom7; overload;
function remove(): JDom7; overload;
(* function removeProp(Const aPropertyName : string) : JDom7; *)
(* Dom7.requestAnimationFrame(callback) - Cross-browser implementation on requestAnimationFrame
   callback - function - A parameter specifying a function to call when it's time to update your animation for the next repaint
   returns animation request id, that uniquely identifies the entry in the callback list  *)
(* function requestAnimationFrame(callback: TProcedure): JDom7; overload; *)
function requestAnimationFrame(callback: TProcedure): Integer; overload;
(* Dom7.cancelAnimationFrame(requestID) - Cancels an animation frame request
   requestID - number - The ID value returned by the call to $$.requestAnimationFrame() that requested the callback  *)
function cancelAnimationFrame(requestID: integer): JDom7; overload;
(* Dom7.serializeObject(object) - Create a serialized representation of a plain object suitable for use in a URL query string
   object - object - Object to serialize
   returns a new unique array  *)
//function serializeObject(obj: variant): JDom7; overload;
function serializeObject(target: JSValue): string; overload;
(* Dom7.toCamelCase(string) - Convert hypens-case string to camelCase string
   string - string - hypens-case string
   returns a new camelCase string *)
//function toCamelCase(str: string): JDom7; overload;
function toCamelCase(aStr: string): string; overload;
(* Dom7.unique(array) - Remove duplicates in passed array
   obj - array - Array to remove duplicates
   returns a new unique array *)
function unique(arr: JSValue): JDom7; overload;
function unique(target: TJSValueDynArray): TJSValueDynArray; overload;

//-------- Shortcuts --------//
function click: JDom7; overload;
function click(handler: JProcEvent): JDom7; overload;
function click(handler: TProcedure): JDom7; overload;

function blur(): JDom7; overload;
function blur(handler: TProcedure): JDom7; overload;
function blur(handler: JProcEvent): JDom7; overload;

function focus(): JDom7; overload;
function focus(handler: TProcedure): JDom7; overload;
function focus(handler: JProcEvent): JDom7; overload;

function focusin(): JDom7; overload;
function focusin(handler: TProcedure): JDom7; overload;
function focusin(handler: JProcEvent): JDom7; overload;

function focusout(): JDom7; overload;
function focusout(handler: TProcedure): JDom7; overload;
function focusout(handler: JProcEvent): JDom7; overload;

function keyup(): JDom7; overload;
function keyup(handler: TProcedure): JDom7; overload;
function keyup(handler: JProcEvent): JDom7; overload;

function keydown(): JDom7; overload;
function keydown(handler: TProcedure): JDom7; overload;
function keydown(handler: JProcEvent): JDom7; overload;

function keypress(): JDom7; overload;
function keypress(handler: TProcedure): JDom7; overload;
function keypress(handler: JProcEvent): JDom7; overload;

function submit(): JDom7; overload;
function submit(handler: TProcedure): JDom7; overload;
function submit(handler: JProcEvent): JDom7; overload;

function change(): JDom7; overload;
function change(handler: TProcedure): JDom7; overload;
function change(handler: JProcEvent): JDom7; overload;

function mousedown(): JDom7; overload;
function mousedown(handler: TProcedure): JDom7; overload;
function mousedown(handler: JProcEvent): JDom7; overload;

function mousemove(): JDom7; overload;
function mousemove(handler: TProcedure): JDom7; overload;
function mousemove(handler: JProcEvent): JDom7; overload;

function mouseup(): JDom7; overload;
function mouseup(handler: TProcedure): JDom7; overload;
function mouseup(handler: JProcEvent): JDom7; overload;

function mouseenter(): JDom7; overload;
function mouseenter(handler: TProcedure): JDom7; overload;
function mouseenter(handler: JProcEvent): JDom7; overload;
function mouseleave(): JDom7; overload;
function mouseleave(handler: TProcedure): JDom7; overload;
function mouseleave(handler: JProcEvent): JDom7; overload;
function mouseout(): JDom7; overload;
function mouseout(handler: TProcedure): JDom7; overload;
function mouseout(handler: JProcEvent): JDom7; overload;
function mouseover(): JDom7; overload;
function mouseover(handler: TProcedure): JDom7; overload;
function mouseover(handler: JProcEvent): JDom7; overload;
function touchstart(): JDom7; overload;
function touchstart(handler: TProcedure): JDom7; overload;
function touchstart(handler: JProcEvent): JDom7; overload;
function touchend(): JDom7; overload;
function touchend(handler: TProcedure): JDom7; overload;
function touchend(handler: JProcEvent): JDom7; overload;
function touchmove(): JDom7; overload;
function touchmove(handler: TProcedure): JDom7; overload;
function touchmove(handler: JProcEvent): JDom7; overload;
function resize(handler: TProcedure): JDom7; overload;
function resize(handler: JProcEvent): JDom7; overload;
function scroll(handler: JProcEvent): JDom7; overload;
function scroll(handler: TProcedure): JDom7; overload;

//-------- Ajax --------//
 (*  $$.ajax(parameters) - Load data from the server
     parameters - object - Request parameters
     returns plain XHR object *)
 //function ajax(parameters: JAjaxSettings): JDom7; overload;
 //function ajax(parameters: JAjaxSettings): JDom7XHR; overload;
class function ajax(aURL: string; aSettings: TJSObject): tJQXHR; overload;
class function ajax(aSettings: TJSObject): TJQXHR; overload;
class function ajax(aSettings: TJSAjaxSettings): TJQXHR; overload;
(* $$.get(url, data, success) - Load data from the server using a HTTP GET request
   url - string - Request url
   data - object - A plain object or string that is sent to the server with the request. Optional
   success - function (data, status, xhr) - A callback function that is executed if the request succeeds. Optional
   returns plain XHR object  *)
//function get(url: string; data: JSValue; success: aCallBack): JDom7; overload;
//function get(url: String; data: JSValue; success: procedure(data: JSValue; status: Double; xhr: JDom7XHR)): JDom7XHR; overload;
function get(aIndex: Integer): TJSElement; overload;
class function get: TJQXHR; overload;
class function get(url: string): TJQXHR; overload;
class function get(url, Data: string): TJQXHR; overload;
class function get(url: string; Data: TJSObject): TJQXHR; overload;
class function get(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler): TJQXHR; overload;
class function get(url, Data: string; success: JDom7AjaxSuccessHandler): TJQXHR;
  overload;
class function get(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler; aDataType: string): TJQXHR; overload;
class function get(url, Data: string; success: JDom7AjaxSuccessHandler;
  aDataType: string): TJQXHR; overload;
class function get(aSettings: TJSAjaxSettings): TJQXHR; overload;
class function get(aSettings: TJSObject): TJQXHR; overload;
(* $$.post(url, data, success) - Load data from the server using a HTTP POST request
   url - string - Request url
   data - object - A plain object or FormData or string that is sent to the server with the request. Optional
   success - function (data, status, xhr) - A callback function that is executed if the request succeeds. Optional
   returns plain XHR object
*)
//function post(url: string; data: variant; success: aCallBack): JDom7; overload;
//function post(url: String; data: Variant; success: procedure(data: Variant; status: Float; xhr: JDom7XHR)): JDom7XHR; overload;
class function post(url: string): TJQXHR; overload;
class function post(url, Data: string): TJQXHR; overload;
class function post(url: string; Data: TJSObject): TJQXHR; overload;
class function post(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler): TJQXHR; overload;
class function post(url, Data: string; success: JDom7AjaxSuccessHandler):
  TJQXHR; overload;
class function post(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler; aDataType: string): TJQXHR; overload;
class function post(url, Data: string; success: JDom7AjaxSuccessHandler;
  aDataType: string): TJQXHR; overload;
class function post(aSettings: TJSAjaxSettings): TJQXHR; overload;
class function post(aSettings: TJSObject): TJQXHR; overload;
(*
  $$.getJSON(url, data, success) - Load JSON-encoded data from the server using a GET HTTP request
  url - string - Request url
  data - object - A plain object or FormData or string that is sent to the server with the request. Optional
  success - function (data, status, xhr) - A callback function that is executed if the request succeeds. Optional
  returns plain XHR object
*)
//function getJSON(url: string; data: variant=undefined; success: variant=undefined): JDom7; overload;
//function getJSON(url: String; data: Variant; success: procedure(data: Variant; status: Float; xhr: JDom7XHR)): JDom7XHR; overload;
class function getJSON(url: string): TJQXHR; overload;
class function getJSON(url, Data: string): TJQXHR; overload;
class function getJSON(url: string; Data: TJSObject): TJQXHR; overload;
class function getJSON(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler): TJQXHR; overload;
class function getJSON(url, Data: string; success: JDom7AjaxSuccessHandler):
  TJQXHR; overload;
class function getJSON(url: string; Data: TJSObject; success:
  JDom7AjaxSuccessHandler; aDataType: string): TJQXHR; overload;
class function getJSON(url, Data: string; success: JDom7AjaxSuccessHandler;
  aDataType: string): TJQXHR; overload;

(*
function addBack(Const aSelector : String) : JDom7; overload;
function addBack : JDom7; overload;
function ajaxComplete(aHandler : JDom7AjaxEventHandler) : JDom7;
function ajaxError(aHandler : JDom7AjaxEventHandler) : JDom7;
function ajaxSend(aHandler : JDom7AjaxEventHandler) : JDom7;
function ajaxStart(aHandler : JDom7AjaxHandler) : JDom7;
function ajaxStop(aHandler : JDom7AjaxHandler) : JDom7;
function ajaxSuccess(aHandler : JDom7AjaxEventHandler) : JDom7;
class procedure ajaxPrefilter(dataTypes : string; aHandler : JDom7AjaxPrefilterHandler); overload;
class procedure ajaxSetup(aSettings : TJSAjaxSettings); overload;
class procedure ajaxSetup(aSettings : TJSObject); overload;
class procedure ajaxTransport(aDataType : string; AHandler : JDom7AjaxTransportHandler);
class function Callbacks : TCallbacks; overload;
class function Callbacks(const aFlags : string) : TCallbacks; overload;
function clearQueue : JDom7; overload;
function clearQueue(const aQueueName : String) : JDom7; overload;
function closest(Const aSelector : String) : JDom7; overload;
function closest(Const aSelector : String; AContext : TJSElement) : JDom7; overload;
function closest(Const aQuery : JDom7) : JDom7; overload;
function closest(Const aElement : TJSElement) : JDom7; overload;
function contents : JDom7;
function dequeue : JDom7; overload;
function dequeue(const aQueueName : String) : JDom7; overload;
class function dequeue(aElement : TJSElement) : JDom7; overload;
class function dequeue(aElement : TJSElement; const aQueueName : String) : JDom7; overload;
function _end : JDom7; external name 'end';
class function escapeSelector(const S : String) : String;
function first : JDom7;
class function getScript(url : String) : TJQXHR; overload;
class function getScript(url : String; aSuccess : JDom7AjaxScriptHandler) : TJQXHR; overload;
function has(Const aSelector : String) : JDom7;
function has(Const aQuery : JDom7) : JDom7;
class function hasData(aElement : TJSElement) : Boolean;
function innerHeight: Integer;
function innerHeight(aValue: Integer) : JDom7;
function innerHeight(aValue: String) : JDom7;
function innerHeight(aHandler: JDom7HeightHandler) : JDom7;
function innerWidth: Integer;
function innerWidth(aValue: Integer) : JDom7;
function innerWidth(aValue: String) : JDom7;
function innerWidth(aHandler: JDom7WidthHandler) : JDom7;
function last : JDom7;
class function load(url : String) : TJQXHR; overload;
class function load(url,Data : String) : TJQXHR; overload;
class function load(url : String; Data : TJSObject) : TJQXHR; overload;
class function load(url : String; Data : TJSObject; success : JDom7AjaxLoadHandler) : TJQXHR; overload;
class function load(url,Data : String; success : JDom7AjaxLoadHandler) : TJQXHR; overload;
function map(aHandler : JDom7MapHandler) : JDom7;
function &not(const aSelector : String) : JDom7; external name 'not'; overload;
function &not(const aSelector : TJSElement) : JDom7; external name 'not'; overload;
function &not(const aSelector : Array of TJSElement) : JDom7; external name 'not'; overload;
function &not(const aSelector : JDom7) : JDom7; external name 'not'; overload;
function &not(const aSelector : JDom7FilterHandler) : JDom7;external name 'not'; overload;
function noConflict : TJSObject; overload;
function noConflict(removeAll: Boolean) : TJSObject; overload;
class function param (aObject : String) : String; overload;
class function param (aObject : TJSObject) : String; overload;
class function param (aObject : JDom7) : String; overload;
class function param (aObject : String; traditional : Boolean) : String; overload;
class function param (aObject : TJSObject; traditional : Boolean) : String; overload;
class function param (aObject : JDom7; traditional : Boolean) : String; overload;
function position : JDom7TopLeft;
class function queue(element : TJSElement) : TJSarray; overload;
class function queue(element : TJSElement; const aQueueName : String) : TJSarray; overload;
class function queue(element : TJSElement; const aQueueName : string; anewQueue : TJSarray) : JDom7; overload;
class function queue(element : TJSElement; const aQueueName : String ; aHandler : JDom7QueueHandler) : JDom7; overload;
function queue : TJSarray; overload;
function queue(aQueueName : string) : TJSarray; overload;
function queue(anArray : TJSArray) : JDom7; overload;
function queue(aQueueName : string; anArray : TJSarray) : JDom7; overload;
function queue(aQueueName : string; aHandler : JDom7AddQueueHandler) : JDom7; overload;
function serialize : string;
function serializeArray : TJSObjectDynArrayArray;
Function siblings : JDom7; overload;
Function siblings(Const aSelector : String) : JDom7; overload;
Function slice(aStart : integer) : JDom7; overload;
Function slice(aStart,aEnd : integer) : JDom7; overload;
Function sub : JDom7;
Function when(APromise : TJSPromise) :  TJSPromise; overload;
Function when :  TJSPromise; overload;
Property ready : TJSPromise Read FReady;
// These should actually be class properties ?
property cssHooks : TJSObject Read FCSSHooks;
property cssNumber : TJSObject read FCSSNumber;
Property Elements[AIndex : Integer] : TJSElement read getEl; default;
*)
  end;

  { ╔════════════════════════════════════════════════╗
    ║ External Global Functions                      ║
    ╚════════════════════════════════════════════════╝ }


function _(const aSelector: TJSDocument): JDom7; external name 'window._';
function _(const aSelector: string): JDom7; external name 'window._';
function _(const aSelector: string; Context: TJSElement): JDom7; external name
  'window._';
function _(const aElement: TJSElement): JDom7; external name 'window._';
function _(const aElement: array of TJSElement): JDom7; external name
  'window._';
function _(const aElement: TJSObject): JDom7; external name 'window._';
function _(const aQuery: JDom7): JDom7; external name 'window._';
function _(): JDom7; external name 'window._';
//Function _(Const aSelector: JSValue) : TJSHTMLElement; external name 'window.Dom7';

var
  g_: JDom7; external name 'window._';
var
  this: TJSElement; external name 'this';

implementation

end.

