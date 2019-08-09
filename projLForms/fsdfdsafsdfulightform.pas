unit ulightform;

(* ╔══════════════════════════════════════╗
   ║  TJSForm PROJECT                        ║
   ║  ----------------------------------- ║
   ║  created by: warleyalex              ║
   ╚══════════════════════════════════════╝ *)
{$mode objfpc}{$H+}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils, uutils;

type
  { TJSi18n }
  TJSForm = class(TObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
    procedure init;
  public
    (* Public declarations *)
    constructor Create;
  published
    (* Published declarations *)
    procedure InitializeObject;
  end;

implementation

{ TJSForm }

constructor TJSForm.Create;
begin
  { ╔═══════════════════════════════════════════════════════════════════════════╗
    ║ Since the document fragment is in memory and not part of the main DOM     ║
    ║ tree, appending children to it does not cause page reflow (computation    ║
    ║ of elements position and geometry). Consequently, using documentfragments ║
    ║ often results in better performance.                                      ║
    ╚═══════════════════════════════════════════════════════════════════════════╝ }

end;

procedure TJSForm.init;
begin

end;

procedure TJSForm.InitializeObject;
begin
  console.log('TJSForm.InitializeObject');
  init;
end;

end.

