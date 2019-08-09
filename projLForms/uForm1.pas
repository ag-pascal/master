unit UForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS, Web,
  //pas2js.Element, pas2js.Form, pas2js.application,
  JComponents, uWButton, uTempl, uFishFacts, uPromises, uVueJS, uStore1
  {Forms, Controls, Graphics, Dialogs};

type
  TForm1 = class(TWForm)
  private
    button1: TButton;
    //modelo1: TTemplate;
    store: TStore;
    FishFacts: TJFishFacts;
    procedure OnLigarClick(sender: TObject);
    procedure OnRightClick(sender: TObject);
    procedure OnLeftClick(sender: TObject);
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  public
  end;

var
  Form1: TForm1;

implementation

ResourceString
t01 =
  '<div>'+
  ' <a v-on:click="a1">Click me!</a>'+
  '  <label for="name">Enter name:</label>'+
  '  <input type="text" v-model="name" id="name" name="name" />'+
  '  <p>{{ name }} is {{ age }} years old.</p>'+
  '  <p>{{ origin | json }}</p>'+
  '</div';

{$R *.lfm}

procedure TForm1.OnLigarClick(sender: TObject);
var
  btnLiga: TJSElement;

begin
  btnLiga := document.querySelector('#smsfish-LIGA');

  { "Ligar / Turn On / Turn Off button }
  document.getElementById('smsfish-ligar').classList.remove('nm');
  document.getElementById('smsfish-hideTable').classList.remove('paused');

  { Turn ON button now is disabled! }
  btnLiga['disabled'] := 'true';

end;

procedure TForm1.OnRightClick(sender: TObject);
begin
  { "R" Right/Next button }
   FishFacts.upClick(Self);
end;

procedure TForm1.OnLeftClick(sender: TObject);
begin
  { "L" Left/Previous button }
  FishFacts.downClick(Self);
end;

function p1(Value: JSValue): JSValue;
//procedure p1(aValue: JSValue);
begin

end;

procedure TForm1.InitializeObject;

  procedure Button1OnClick(sender: TObject);
  begin
    console.log('TForm1 button1 clicked');
    Application.GoToForm('Form2');

  end;

  procedure rolaPreta(aTime: TJSDOMHighResTimeStamp);
  begin
    console.log('Form1',  aTime);
    button1.Handle.style.setProperty('position', 'absolute');
  end;

  procedure p;
  begin
    //console.log(TJSObject(modelo1.data['dados'])['origin']);
  end;

  procedure onReady;

    procedure _f(response: JSValue);
    begin
      console.log(response);
      &set('origin', response);
      // this.$set('origin', response)
  asm
    this.ip = response.origin;
    this.origin = response
  end;
    end;

    procedure _e(data, status, request: JSValue);
    begin
    // handle error
      console.error(data);
    end;

  begin
   http.get('http://httpbin.org/ip', @_f)
     .error(@_e);
  end;

  procedure onComputed;
  begin
     ASM
     //return this.name
     end;
  end;

procedure onLog;
begin
   ASM
   //return this.name
    console.log( this.$data.name );
    console.log( this.$data.age );
    console.log( this.$data.origin );
   end;
end;

begin
  inherited;
  console.log('TForm1.InitializeObject');
  store:= TStore.Create(Self);


  FishFacts := TJFishFacts.Create(Self);
  FishFacts.SetBounds(250, 20, 150, 150);
  FishFacts.OnLigarClick:= @OnLigarClick;
  FishFacts.OnRightClick:= @OnRightClick;
  FishFacts.OnLeftClick:= @OnLeftClick;

 //console.log(store.dataset);


  //FishFacts.InitializeObject;

  button1:= TButton.Create(Self);
  button1.SetBounds(350, 10, 150, 150);
  button1.Caption:='Goto F2';
  button1.OnClick := @Button1OnClick;
  button1.ReadyExecute(@rolapreta);

(*
  modelo1:= TTemplate.Create(Self, t01);
  modelo1.SetBounds(50, 20, 150, 150);
  modelo1.Handle.style.setProperty('background-color', 'gainsboro');
  modelo1.Handle.style.setProperty('position', 'absolute');
  modelo1.data := new([
                        'name', 'warleyalex',
                        'age', 74,
                        'ip', '',
                        'origin', nil
                      ]);
  modelo1.methods['a1'] := @Button1OnClick;
  modelo1.ready:= @onReady;
  modelo1.computed['currentDashboard']:=@onComputed;
  modelo1.Enabled:=true;

  modelo1.methods['a2'] := @onReady;
  modelo1.methods['log'] := @onLog;

  //window.setTimeout(@p, 1000);
//  console.log(TJSObject(modelo1.o.Properties['data']));
   // console.log(TJSObject(modelo1.o.Properties['data']));
   console.log(vueInstance);
*)

//console.log(vueInstance);
end;

procedure TForm1.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  console.log('TForm1.InitializeForm');
  //console.log(vueInstance);
end;

procedure TForm1.Resize;
begin
  inherited;
  console.log('TForm1.Resize');
end;

end.

