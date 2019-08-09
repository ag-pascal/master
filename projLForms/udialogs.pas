unit uDialogs;

(* ╔═════════════════════════════╗
   ║ created by: warleyalex      ║
   ╚═════════════════════════════╝
*)
{$mode objfpc}{$H+}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, Types, JS, Web;

type
  aCallBack =  procedure(data: JSValue) of object;
  aCancelCallBack  =  procedure(data: JSValue) of object;
  aLoginCallBack = procedure(username, password: string) of object;
  aPasswordOKCallBack = procedure(password: string) of object;
  TProcedure = procedure of object;

type
  TDialog = class external name 'pas2js.dialogs'
  public
    constructor New; overload;
    function modal(params: JSValue): TDialog;
    function alert(text: string): TDialog; overload;
    function alert(text: string; title: string): TDialog; overload;
    function alert(text: string; callbackOk: TProcedure): TDialog; overload;
    function alert(text: string; title: string; callbackOk: TProcedure): TDialog; overload;

    function confirm(text: string): TDialog; overload;
    function confirm(text: string; title: string): TDialog; overload;
    function confirm(text: string; callbackOk: TProcedure): TDialog; overload;
    function confirm(text: string; title: string; callbackOk: TProcedure): TDialog; overload;
    function confirm(text: string; callbackOk: TProcedure; callbackCancel: TProcedure): TDialog; overload;
    function confirm(text: string; title: string; callbackOk: TProcedure; callbackCancel: TProcedure): TDialog; overload;


    function prompt(text: string): TDialog; overload;
    function prompt(text: string; title: string): TDialog; overload;
    function prompt(text: string; callbackOk: aCallBack): TDialog; overload;
    function prompt(text: string; callbackOk: aCallBack; callbackCancel: aCancelCallBack): TDialog; overload;
    function prompt(text: string; title: string; callbackOk: aCallBack): TDialog; overload;
    function prompt(text: string; title: string; callbackOk: aCallBack; callbackCancel: aCancelCallBack): TDialog; overload;

    function modalLogin(text: string): TDialog; overload;
    function modalLogin(text: string; title: string): TDialog; overload;
    function modalLogin(text: string; title: string; callbackOk: aLoginCallBack): TDialog; overload;
    function modalLogin(text: string; callbackOk: aLoginCallBack): TDialog; overload;
    function modalLogin(text: string; title: string; callbackOk: aLoginCallBack; callbackCancel: aLoginCallBack): TDialog; overload;

    function modalPassword(text: string): TDialog; overload;
    function modalPassword(text: string; title: string): TDialog; overload;
    function modalPassword(text: string; title: string; callbackOk: aPasswordOKCallBack): TDialog; overload;
    function modalPassword(text: string; callbackOk: aPasswordOKCallBack): TDialog; overload;
    function modalPassword(text: string; title: string; callbackOk: aPasswordOKCallBack; callbackCancel: TProcedure): TDialog; overload;


    function showPreloader: TDialog; overload;
    function showPreloader(text: string): TDialog; overload;
    function hidePreloader(): TDialog;

    function showIndicator(): TDialog;
    function hideIndicator(): TDialog;

    function actions(params: JSValue): TDialog;
    function openModal(modal: JSValue): TDialog;
    function closeModal(modal: JSValue): TDialog;
end;

implementation

end.

