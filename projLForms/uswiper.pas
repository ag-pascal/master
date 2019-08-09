unit uSwiper;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Types, JS, Web;

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

end.

