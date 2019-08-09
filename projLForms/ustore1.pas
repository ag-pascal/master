unit uStore1;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils, DB, JSONDataSet, jComponents,
  uTempl, uPromises, uVueJS, uCDS;

type
  { TJFishFacts }

  { TStore }

  TStore = class(TMovableControl)
  private
    (* Private declarations *)
    modelo1: TTemplate; external name 'store';
    fdataset: JSValue; external name 'store.fo.data';
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)
    constructor Create(AOwner: TControl); override;

  published
    (* Published declarations *)
    property dataset: JSValue read fdataset write fdataset;
  end;

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

{ TStore }

constructor TStore.Create(AOwner: TControl);

  procedure Button1OnClick(sender: TObject);
  begin
    console.log('button1 clicked');
  end;

  procedure p;
  begin
    console.log(TJSObject(modelo1.data['dados'])['origin']);
  end;

  procedure onReady;

    procedure _f(response: JSValue);
    begin
      //console.log(response);
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
  inherited Create(AOwner);

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
  // console.log(vueInstance);


end;

end.

