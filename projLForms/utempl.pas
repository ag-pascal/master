unit uTempl;

{$mode objfpc}{$H+}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, Types, JS, Web, uVueJS, JComponents;

type
  (*
   ╔═════════════════════════════╗
   ║ TTemplate component         ║
   ╚═════════════════════════════╝ *)

  { TTemplate }

  TTemplate = class(TMovableControl)
  private
  { private declarations }
    el: TJSNode;
    fo: TDirectives;
    fdata: TJSObject; external name 'fo.data';
    fmethods: TJSObject; external name 'fo.methods';
    fready: JSValue; external name 'fo.ready';
    fcomputed: TJSObject; external name 'fo.computed';
    frun: JSValue; external name 'fo';
    ftemplate: string;
    fEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
  protected
  { protected declarations }
  public
  { public declarations }
    constructor Create(AOwner: TControl; TemplateName: String);
    property data: TJSObject read fdata write fdata;
    property methods: TJSObject read fmethods write fmethods;
    property ready: JSValue read fready write fready;
    property computed: TJSObject read fcomputed write fcomputed;
    o: TJSObject; external name 'fo';
    //procedure execute;
  published
  { published declarations }
  { specifies whether the script is enabled }
     property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

//  function parseHTML(html: string): TJSNode;

implementation

function parseHTML(html: string): TJSNode;
var
  t : TSJHTMLTemplateElement;
begin
  t := TSJHTMLTemplateElement(document.createElement('template'));
  t.innerHTML := html;
  result := t.content.cloneNode(true);
end;

{ Template1 }

constructor TTemplate.Create(AOwner: TControl; TemplateName: String);
begin
  inherited Create(AOwner);
  Self.ftemplate :=  TemplateName;
  el := parseHTML(  {t1} Self.ftemplate );
  Self.Handle.appendChild(el);

  fo := TDirectives.New;
  fo.el := '.swiper-container';
  fo.methods := TJSObject.New;
  fo.computed:= TJSObject.New;
  //fo.data := TJSObject.New;

  InitializeObject;
end;

procedure TTemplate.SetEnabled(aValue: Boolean);
begin
  if (aValue <> FEnabled) then
  begin
    if aValue then
    begin
      //FreeElement;
      //CreateElement;
      vueInstance := JVue.New ( Self.frun );
    end else
      FEnabled := aValue;
  end;
end;

(*procedure TTemplate.execute;
begin
  JVue.New ( Self.frun );
end;*)

end.

