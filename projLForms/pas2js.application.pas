{
	This is free and unencumbered software released into the public domain.

	Anyone is free to copy, modify, publish, use, compile, sell, or
	distribute this software, either in source code form or as a compiled
	binary, for any purpose, commercial or non-commercial, and by any
	means.

	In jurisdictions that recognize copyright laws, the author or authors
	of this software dedicate any and all copyright interest in the
	software to the public domain. We make this dedication for the benefit
	of the public at large and to the detriment of our heirs and
	successors. We intend this dedication to be an overt act of
	relinquishment in perpetuity of all present and future rights to this
	software under copyright law.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
	OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
	ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
}
unit pas2js.Application;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.Form;


(* ╔═══════════════════════════════════════════════════════════════════════╗
   ║ JSwiper                                                               ║
   ╚═══════════════════════════════════════════════════════════════════════╝ *)
type
  JSwiper = class external name 'Swiper'
    constructor New(targetRef: String); overload;

    function _slideNext: JSwiper;
    function swipeNext: JSwiper;
    function _slideTo(form: integer): JSwiper;
    function swipeTo(form: integer): JSwiper; overload;
    function swipeTo(form: integer; speed: Integer): JSwiper; overload;
    function swipeTo(form: integer; speed: Integer; callbackFn: TProcedure): JSwiper; overload;
  end;

type
  { TWApplication }
  TWApplication = class
  private
    FOldForm: Integer;
    FForm: TCustomControl;
  protected
    procedure ApplicationStarting; virtual;
  public
    FormNames: Array of String;
    FormsClasses: Array of TFormClass;       //TFormClass = class of TWForm;
    FormsInstances: Array of TWForm;
    Swiper: JSwiper;
    constructor Create; virtual;
    procedure CreateForm(FormName: String; aClassType: TFormClass);
    //procedure GoToForm(FormName: String);
    procedure GoToForm(aFormName: String; speed: integer = 350; aCallBackFn: TProcedure = nil);
    procedure RunApp;
    property CurrentForm: Integer read FOldForm write FOldForm;
  end;

var
  Application : TWApplication;

implementation

{ TWApplication }

procedure TWApplication.ApplicationStarting;
begin
  // empty
end;

constructor TWApplication.Create; //(parent: TCustomControl);
var
  docFragment: TJSDocumentFragment;
  div_, div_0: TJSElement;

  procedure doOnReadyExecute;
  begin
    console.log('onReadyExecute');
    (* swiper enabled after forms created... *)
    if not Assigned(Swiper) then
    Swiper := JSwiper.New('.swiper-container');

  end;

begin
  docFragment := document.createDocumentFragment(); // contains all gathered nodes
  div_ := document.createElement('DIV');
  div_.setAttribute('class', 'swiper-container');
  div_.setAttribute('style', 'width:100%; height:'+IntToStr(window.screen.height)+'px;');
  //div_.setAttribute("style", 'width:100%; height:'+window.screen.height+'px;'+ ' position:relative;');
  docFragment.appendChild(div_);

  div_0 := document.createElement('DIV');
  div_0.setAttribute('class', 'swiper-wrapper');
  div_0.setAttribute('style', 'width:100%; height:100%;');
  div_.appendChild(div_0);
  document.body.appendChild( docFragment );

 // inherited Create('div', parent);

//  Self.Handle.classList.add('swiper-container');
//  Self.Handle.setAttribute('style', 'width:100%; height:'+IntToStr(window.screen.height)+'px;');



  //setProperty('width','100%');
  //setProperty('height','100%');
  //setProperty('background-color','white');

  (* set class instance variable to Display *)
  // if not Assigned(FForm) then
   FForm := TCustomControl.Create;
   TJSElement(FForm.Handle) := TJSHTMLElement( document.querySelector('.swiper-container')).firstElementChild;

   window.setTimeout(@doOnReadyExecute, 250);
  (* swiper enabled after forms created... *)
//  if not Assigned(Swiper) then
//  Swiper := JSwiper.New('.swiper-container');
//  (* set class instance variable *)
//  if not Assigned(Instance) then
//    Instance := Self;

end;

procedure TWApplication.CreateForm(FormName: String; aClassType: TFormClass);
var
  k: integer;
begin
  TJSArray(FormNames).push(FormName);
  TJSArray(FormsClasses).push(aClassType);
  TJSArray(FormsInstances).push(nil);

  for k := 0 to TJSArray(FormNames).Length -1 do begin
    If FormsInstances[k] = nil then begin
      FormsInstances[k] := FormsClasses[k].Create(FForm);      // create the form instances
      (FormsInstances[k]).FormIndex := k;   // set Form Index
      (FormsInstances[k]).Name := formName;
      (FormsInstances[k]).FormActivated;    // invoke initializeForm method --> invoke FormActivated method
      (FormsInstances[k]).InitializeObject;
    end;
  end;
end;

(*
,Show:function(Self, FormName$1) {
   var i = 0;
   var $temp1;
   for(i=0,$temp1=Self.FormNames.length;i<$temp1;i++) {
      if (Self.FormsInstances[i]!==null) {
         TCustomControl.SetProperty(Self.FormsInstances[i],"display","none");
      }
      if (Self.FormNames[i]==FormName$1) {
         if (Self.FormsInstances[i]===null) {
            Self.FormsInstances[i]=TWForm.Create$5($NewDyn(Self.FormsClasses[i],""),Self);
            TWForm.ShowForm$(Self.FormsInstances[i]);
         } else {
            TWForm.ClearForm(Self.FormsInstances[i]);
            TWForm.ShowForm$(Self.FormsInstances[i]);
            TCustomControl.SetProperty(Self.FormsInstances[i],"display","inline-block");
         }
      }
   }
}
*)
//procedure TWApplication.GoToForm(FormName: String);
procedure TWApplication.GoToForm(aFormName: String; speed: integer;
  aCallBackFn: TProcedure);

var
  i: integer;

  (* ╔═════════════════════════════════════════════════════════════╗
     ║ pass the FormName parameter and return the FormIndex        ║
     ╚═════════════════════════════════════════════════════════════╝ *)
    function IndexOfFormName(aformName: String): Integer;
    var
      lcName: String;
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
  n: String;
begin
//  for i := 0 to TJSArray(FormNames).Length - 1 do
//  console.log(IndexOfFormName('Form2'));

  (* ╔══════════════════════════════════════════════════════════╗
     ║ Invoke FormDeactivate event before SwipeTo/slideTo page  ║
     ╚══════════════════════════════════════════════════════════╝ *)
  for i := 0 to TJSArray(FormNames).Length - 1 do begin
    if Trim(LowerCase(FormsInstances[i].Name)) = Trim(LowerCase(getForm(FOldForm).Name)) then begin
      FormsInstances[i].FormDeactivated; // ---> invoke FormDeactivated
    end;
  end;

  If assigned(Swiper) then
    Application.Swiper.swipeTo(IndexOfFormName(aFormName), speed, aCallBackFn);

  (* ╔══════════════════════════════════════════════════════════╗
     ║ Invoke FormActivate event after SwipeTo/slideTo page     ║
     ╚══════════════════════════════════════════════════════════╝ *)

  n := Trim(LowerCase(aFormName));
  for i := 0 to TJSArray(FormNames).Length - 1 do begin
    if Trim(LowerCase(FormsInstances[i].Name)) = Trim(LowerCase(aFormName)) then begin
      FormsInstances[i].FormActivated; // ---> invoke FormActivated
      FOldForm := FormsInstances[i].FormIndex;
    end;
  end;


   (*
  For i := 0 to TJSArray(FormNames).length -1 do begin
    If FormsInstances[i] <> nil then
      FormsInstances[i].SetProperty('display','none');
    If FormNames[i] = aFormName then begin
      If FormsInstances[i] = nil then       //form has never been displayed yet
        FormsInstances[i] := FormsClasses[i].Create(FForm) else
        FormsInstances[i].SetProperty('display','inline-block');

      //TW3Form(FormsInstances[i]).InitializeForm;    //ClearForm;
      //*this.FormsClasses[i](this.FormsInstances[i])
      TWForm(FormsInstances[i]).InitializeForm;
      TWForm(FormsInstances[i]).InitializeObject;
        //(FormsInstances[i] as FormsClasses[i]).InitializeForm;    //ClearForm;
        //(FormsInstances[i] as FormsClasses[i]).InitializeObject;  //ShowForm;
    end;
  end;
 *)
end;

procedure TWApplication.RunApp;
begin
  console.log('TWApplication.RunApp');
  ApplicationStarting;
end;

initialization
//Application := TWApplication.Create(nil);
//Application.RunApp;

end.

