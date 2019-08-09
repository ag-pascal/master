unit uinternationalization;

(* ╔══════════════════════════════════════╗
   ║  i18n PROJECT                        ║
   ║  ----------------------------------- ║
   ║  created by: warleyalex              ║
   ╚══════════════════════════════════════╝ *)
{$mode objfpc}{$H+}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils, uutils, unit1;

type
  { TJSi18n }
  TJSi18n = class(TObject)
  private
    (* Private declarations *)
    xhttp: TJSXMLHttpRequest;
    langDocument: JSValue;
    //procedure bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
  protected
    (* Protected declarations *)
    procedure init;
    procedure switchLanguage(language: JSValue);
    procedure processLangDocument;
  public
    (* Public declarations *)
    constructor Create;

    function DoFormLoad(Event: TEventListenerEvent): boolean;
    function DoFormAbort(Event: TEventListenerEvent): boolean;
  published
    (* Published declarations *)
    procedure InitializeObject;
  end;

implementation

{ TJSi18n }

constructor TJSi18n.Create;
begin
  { ╔═══════════════════════════════════════════════════════════════════════════╗
    ║ Since the document fragment is in memory and not part of the main DOM     ║
    ║ tree, appending children to it does not cause page reflow (computation    ║
    ║ of elements position and geometry). Consequently, using documentfragments ║
    ║ often results in better performance.                                      ║
    ╚═══════════════════════════════════════════════════════════════════════════╝ }

end;

procedure TJSi18n.processLangDocument;
var
  tags: TJSNodeList;
  l: TJSArray;
  i: integer;

  function LforEach(value: JSValue; index: NativeInt; anArray : TJSArray) : Boolean;
  var
    key: String;
  begin
    key := String( TJSObject(TJSHTMLElement(value).dataset).Properties['languagekey'] );

    if(  TJSObject(langDocument)[key] ) then
      TJSElement (l.Elements[index]).innerText:= String( TJSObject(langDocument)[key] );
  end;

begin
  tags := document.querySelectorAll('span,img,a,label,li,option,h1,h2,h3,h4,h5,h6');
  l:= TJSArray.new;
  for i:=0 to tags.length - 1 do
    l.push(tags[i]);
  l.forEach( @LforEach );
end;

function TJSi18n.DoFormLoad(Event: TEventListenerEvent): boolean;
begin
  if (xhttp.status = 200) and (xhttp.readyState = 4) then
  begin
   // If successful
   langDocument := TJSJSON.parse(xhttp.responseText);
   processLangDocument();
  end;

  result := true;
end;

function TJSi18n.DoFormAbort(Event: TEventListenerEvent): boolean;
begin
  //ShowMessage('Failed to load form HTML template file');
  WriteLn('Failed to load form HTML template file');
  Result := true;
end;

procedure TJSi18n.init;
var
  tmp: TJSHTMLCollection;
  languages: TJSArray;
  i: integer;
  el: TJSHTMLElement;

  procedure languageCallBack(event: TJSEvent);
  begin
    switchLanguage (
      (TJSHTMLElement(event.target).dataset).Properties['lang']
    );
  end;

  function languageforEach(value: JSValue; index: NativeInt; anArray : TJSArray) : Boolean;
  begin
    JElement (languages.Elements[index]).addEventListener('click', @languageCallBack);
  end;

begin
  tmp := document.getElementsByClassName('language');
  languages := TJSArray.new();
  for i:=0 to tmp.Length - 1 do
  languages.push(tmp[i]);

  languages.forEach( @languageforEach );
end;

procedure TJSi18n.switchLanguage(language: JSValue);
begin
  xhttp := TJSXMLHttpRequest.new();
  xhttp.addEventListener('load', @DoFormLoad);
  xhttp.addEventListener('abort', @DoFormAbort);
  xhttp.open('GET', 'i18n/' + String(language) + '.json', true);
  xhttp.setRequestHeader('Content-type','application/json');
  xhttp.setRequestHeader('Cache-Control','no-cache');
  xhttp.send();
end;

procedure TJSi18n.InitializeObject;
begin
  console.log('i18n1.InitializeObject');
  init;
end;

end.

