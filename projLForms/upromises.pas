unit uPromises;

{$MODE objfpc}
{$MODESWITCH externalclass}

interface

  uses JS, Web;

type
  TJSValueDynArray = array of JSValue;
  JDeferred = class;
  TJPromiseCallback = procedure(Value: JSValue);
  TJDeferredObject_fn = function(d: TJPromiseCallback): JSValue;
  TJDeferredObject = procedure(resolve: TJPromiseCallback; reject: TJPromiseCallback);
  TJPromiseCallback_fn = function(Value: JSValue): JSValue;
  TJDeferredEventHandler = function(event: JSValue): JSValue;
  TJPromiseError = procedure(data, status, request: JSValue) of object;

  JPromise = class external name 'Promise'
    constructor new(fn: TJDeferredObject_fn { = nil}); overload;
    constructor new(resolve: TJDeferredObject_fn; reject: TJDeferredObject_fn); overload;
    constructor new(fn: TJDeferredObject); overload;
    function always(alwaysCallbacks: TJSValueDynArray): JPromise;
    function done(doneCallbacks: TJSValueDynArray): JPromise; overload;
    function done(doneCallbacks: JSValue): JPromise; overload;
    procedure error(proc: TJPromiseError);
    function fail(failCallbacks: TJSValueDynArray): JPromise;
    function progress(progressCallbacks: TJSValueDynArray): JPromise;
    function state(): string;
    function &then(doneCallbacks: JSValue; failCallbacks: JSValue{ = undefined};
      progressCallbacks: JSValue { = undefined}): JPromise; external name 'then';
    function &then(onFulfilled: TJPromiseCallback_fn = nil): JPromise; overload;
      external name 'then';
    function &then(onFulfilled: TJPromiseCallback_fn; onRejected:
      TJPromiseCallback_fn): JPromise; overload; external name 'then';
    function &then(onFulfilled: TJPromiseCallback; onRejected:
      TJPromiseCallback): JPromise; overload; external name 'then';
    function catch(rejecTJPromiseCallback: JSValue = nil): JPromise; overload;
    function catch(rejecTJPromiseCallback: TJPromiseCallback_fn): JPromise; overload;
    class function promise(target: JSValue): JPromise;
  end;

type
  JDeferred = class external name 'Promise'(JPromise)
    function notify(args: TJSValueDynArray): JDeferred;
    function notifyWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function reject(args: TJSValueDynArray): JDeferred; overload;
    function reject(args: JSValue): JDeferred; overload;
    function reject(args: TJDeferredEventHandler): JDeferred; overload;
    function rejectWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function resolve(args: TJSValueDynArray): JDeferred; overload;
    function resolve(value: JSValue = nil): JPromise; overload;
    function resolveWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function all(iterable: TJSValueDynArray): JPromise; overload;
    function all(iterable: TJSArray): JPromise; overload;
    function race(iterable: TJSValueDynArray): JPromise;
  end;

  { global external functions }
var Promise: JDeferred; external name 'Promise';// property;
var queue: JPromise; external name 'Promise.resolve()';
function Error(message: JSValue): JSValue; external name 'Error';
function wait(ms: NativeInt): JPromise;
function getURI(url: string): JPromise; //JSValue;
function myRequire( url: string): JSValue;

implementation

function wait(ms: NativeInt): JPromise;
  function setTimeout(ahandler : TJPromiseCallback; aTimeout : Integer): Integer; varargs;  external name 'window.setTimeout';

  procedure p(resolve, reject: TJPromiseCallback);
  begin
    setTimeout(resolve, ms);
  end;

begin
  result := JPromise.new(@p);
end;

function getURI(url: string): JPromise; //JSValue;
var
  request: TJSXMLHttpRequest;

  procedure p(resolve: TJPromiseCallback; reject: TJPromiseCallback);
  // Standard XHR to load an image
    procedure doOnLoad;
    begin
      // This is called even on 404 etc
      // so check the status
      if (request.status = 200) then
      begin
       // If successful, resolve the promise by passing back the request response
       resolve(request.response);
      end
      else
      begin
        // Otherwise reject with the status text
        // which will hopefully be a meaningful error
        reject(Error('File didn''t load successfully; error code: ' + request.statusText));
      end;
    end;

    procedure doOnError;
    begin
      // Also deal with the case when the entire request fails to begin with
      // This is probably a network error, so reject the promise with an appropriate message
      reject(Error('There was a network error.'));
    end;

  Begin
    request := TJSXMLHttpRequest.new;
    request.open('GET', url);

    // When the request loads, check whether it was successful
    request.addEventListener('load', @doOnLoad);
    // Handle network errors
    request.addEventListener('abort', @doOnError);
    // Send the request
    request.send();
  End;

begin
// Create new promise with the Promise() constructor;
// This has as its argument a function
// with two parameters, resolve and reject
  Result := JPromise.new(@p);
end;

function myRequire( url: string): JSValue;
  var
    ajax: TJSXMLHttpRequest;

  function ev(win: TJSWindow; arr: array of JSValue): JSValue; external name 'eval.apply';

  procedure onReady;
  var
    script: JSValue;
  begin
    script := ajax.response; // ?? ajax.responseText;
    if (ajax.readyState = 4) then begin
      case( (ajax.status)) of
      200: begin
        //eval.apply( window, [script] );
        ev( window, [script] );
        console.log('script loaded: '+ url);
      end else
        console.log('ERROR: script not loaded: '+ url);
      end;
    end;
  end;


begin
  ajax := TJSXMLHttpRequest.New;
  ajax.open( 'GET', url, false ); // <-- the 'false' makes it synchronous
  ajax.onreadystatechange := @onReady;
  ajax.send(null);
end;


end.

