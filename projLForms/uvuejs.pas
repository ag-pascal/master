unit uVueJS;

{$mode objfpc}{$H+}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, Types, Math, JS, Web, uDom, uPromises;

type
 TFunction = function(value: JSValue): JSValue;

 TProc = procedure(data: JSValue) of object;



type
  TVueResource = class external name 'this.$http'
    public
     function get(url: String; callback: TProc): JPromise;
     function post(url: String; callback: TProc): JPromise;
     function put(url: String; callback: TProc): JPromise;
     function patch(url: String; callback: TProc): JPromise;
     function delete(url: String; callback: TProc): JPromise;
     function jsonp(url: String; callback: TProc): JPromise;
  end;

  function &set(exp: String; val: JSValue): JSValue; external name 'this.$set';
  var
    http: TVueResource; external name 'this.$http';

type
  TDirectives = class external name 'Object'
    constructor New; overload;
    _component: JSValue;
    _prop: JSValue;
    attr: JSValue;
    &class: TFunction;
    cloak: JSValue;
    data: TJSObject;
    el: JSValue;
    methods: TJSObject;
    html: JSValue;
    &if: JSValue;
    model: JSValue;
    &on: JSValue;
    ref: JSValue;
    &repeat: JSValue;
    show: TFunction;
    style: JSValue;
    templates: JSValue;
    text: JSValue;
    transition: JSValue;
    ready: JSValue;
    computed: TJSObject;
  end;




type
  JVue = class external name 'Vue'
    constructor New(instanceOpt: JSValue); overload;
    constructor New(instanceOpt: TDirectives); overload;
//    class function &set(exp: String; val: JSValue): JSValue; external name 'this.$set';
    //class var cid: Float;
    //class var compiler: compile: (el: JSValue; options: JSValue; partial: JSValue; transcluded: JSValue): JSValue;
    //  compileAndLinkRoot: (vm: JSValue; el: JSValue; options: JSValue): JSValue;
    //  transclude: (el: JSValue): JSValue;
    //end;
    //class var component: (id: JSValue; definition: JSValue): JSValue;
    //class var config:       _assetTypes: array of String;
    //  _delimitersChanged: Boolean;
    //  _propBindingModes: JSValue;
    //  async: Boolean;
    //  debug: Boolean;
    //  delimiters: String;
    //  interpolate: Boolean;
    //  prefix: String;
    // proto: Boolean;
    // silent: Boolean;
    //  warnExpressionErrors: Boolean;
//    end;
    //class var directive: (id: JSValue; definition: JSValue): JSValue;
    //class var elementDirective: (id: JSValue; definition: JSValue): JSValue;
    //class var extend: (extendOptions: JSValue): JSValue;
    //class var filter: (id: JSValue; definition: JSValue): JSValue;
    //class var nextTick: (cb: JSValue; ctx: JSValue): JSValue;
    //class var options:       components: JSValue;
(*
      directives: record
        "_component": JSValue;
        "_prop": JSValue;
        "attr": JSValue;
        "class": (value: JSValue): JSValue;
        "cloak": JSValue;
        "el": JSValue;
        "html": JSValue;
        "if": JSValue;
        "model": JSValue;
        "on": JSValue;
        "ref": JSValue;
        "repeat": JSValue;
        "show": (value: JSValue): JSValue;
        "style": JSValue;
        "text": JSValue;
        "transition": JSValue;
      end;
      elementDirectives: record
        "content": JSValue;
      end;
      filters: record
        "capitalize": (value: JSValue): JSValue;
        "currency": (value: JSValue; sign: JSValue): JSValue;
        "filterBy": (arr: JSValue; searchKey: JSValue; delimiter: JSValue; dataKey: JSValue): JSValue;
        "json": JSValue;
        "key": (handler: JSValue; key: JSValue): JSValue;
        "lowercase": (value: JSValue): JSValue;
        "orderBy": (arr: JSValue; sortKey: JSValue; reverseKey: JSValue): JSValue;
        "pluralize": (value: JSValue): JSValue;
        "uppercase": (value: JSValue): JSValue;
      end;
      transitions: JSValue;
    end;
*)
    //class var parsers: JSValue;
    //class var transition: (id: JSValue; definition: JSValue): JSValue;
    //class var use: (plugin: JSValue): JSValue;
    //class var util:       Vue: JVue;
      //addClass: (el: JSValue; cls: JSValue): JSValue;
      //after: (el: JSValue; target: JSValue): JSValue;
      animationEndEvent: String;
      animationProp: String;
      //assertAsset: (val: JSValue; &type: JSValue; id: JSValue): JSValue;
      //assertProp: (prop: JSValue; value: JSValue): JSValue;
      //attr: (node: JSValue; attr: JSValue): JSValue;
      //before: (el: JSValue; target: JSValue): JSValue;
      //bind: (fn: JSValue; ctx: JSValue): JSValue;
      //camelize: (str: JSValue): JSValue;
      //cancellable: (fn: JSValue): JSValue;
      //checkComponent: (el: JSValue; options: JSValue): JSValue;
      //classify: (str: JSValue): JSValue;
      //createAnchor: (content: JSValue; persist: JSValue): JSValue;
      //debounce: (func: JSValue; wait: JSValue): JSValue;
      //define: (obj: JSValue; key: JSValue; val: JSValue; enumerable: JSValue): JSValue;
      //extend: (to: JSValue; from: JSValue): JSValue;
      //extractContent: (el: JSValue; asFragment: JSValue): JSValue;
      //hasProto: Boolean;
      //inBrowser: Boolean;
      //inDoc: (node: JSValue): JSValue;
      //indexOf: (arr: JSValue; obj: JSValue): Float;
      //isAndroid: Boolean;
      //isArray: (obj: JSValue): JSValue;
      //isIE9: Boolean;
      //isObject: (obj: JSValue): JSValue;
      //isPlainObject: (obj: JSValue): JSValue;
      //isReserved: (str: JSValue): JSValue;
      //isTemplate: (el: JSValue): JSValue;
      //log: (msg: JSValue): JSValue;
      //mergeOptions: (parent: JSValue; child: JSValue; vm: JSValue): JSValue;
      //nextTick: (cb: JSValue; ctx: JSValue): JSValue;
      //off: (el: JSValue; event: JSValue; cb: JSValue): JSValue;
      //on: (el: JSValue; event: JSValue; cb: JSValue): JSValue;
      //prepend: (el: JSValue; target: JSValue): JSValue;
      //remove: (el: JSValue): JSValue;
      //removeClass: (el: JSValue; cls: JSValue): JSValue;
      //replace: (target: JSValue; el: JSValue): JSValue;
      //resolveAsset: (options: JSValue; &type: JSValue; id: JSValue): JSValue;
      //stripQuotes: (str: JSValue): JSValue;
      //toArray: (list: JSValue; start: JSValue): JSValue;
      //toBoolean: (value: JSValue): JSValue;
      //toNumber: (value: JSValue): JSValue;
      //toString: (value: JSValue): JSValue;
      //transitionEndEvent: String;
      //transitionProp: String;
      //warn: (msg: JSValue): JSValue;
    //end;
(*
    $: JSValue;
    $$: JSValue;
    $data: JSValue;
    $children: JArray{<JVue>};
    $el: JHTMLElement;
    $options: JSValue;
    $parent: JVue;
    $root: JVue;
    $add: (key: String; val: JSValue): JSValue;
    $addChild: (options: JSValue; &constructor: JFunction): JSValue;
    $after: (target: JSValue {JHTMLElement or String}; cb: JFunction): JSValue;
    $appendTo: (target: JSValue {JHTMLElement or String}; cb: JFunction): JSValue;
    $before: (target: JSValue {JHTMLElement or String}; cb: JFunction): JSValue;
    $broadcast: (event: String; args: JArray{<JSValue>}): JSValue;
    $compile: (el: JHTMLElement): JSValue;
    $delete: (key: String): JSValue;
    $destroy: (remove: Boolean): JSValue;
    $dispatch: (event: String; args: JArray{<JSValue>}): JSValue;
    $emit: (event: String; args: JArray{<JSValue>}): JSValue;
    $eval: (text: String): JSValue;
    $get: (exp: String): JSValue;
    $interpolate: (text: String): JSValue;
    $log: (path: String): JSValue;
    $mount: (el: JSValue {JHTMLElement or String}): JSValue;
    $nextTick: (fn: JFunction): JSValue;
    $off: (event: String; fn: JFunction): JSValue;
    $on: (event: String; fn: JFunction): JSValue;
    $once: (event: String; fn: JFunction): JSValue;
    $remove: (cb: JFunction): JSValue;
    $set: (exp: String; val: JSValue): JSValue;
    $watch: (exp: JSValue {String or JFunction}; cb: (val: JSValue; old: JSValue): JSValue; options: record
      deep: Boolean; // nullable
      immediate: Boolean; // nullable
    end): JSValue;

*)
  end;
(*
type
 JObj = class external 'object'
   Common: integer;
 end;

 JChild= class external (JObj)
   Concretion: string;
 end;

type
  JClassOne = partial class external 'ClassOne';

  JClassTwo = partial class external 'ClassTwo' (JClassOne)
  public

  end;

  JClassOne = class external 'ClassOne'
  public

  end;
//-----------------------------------------------

TRecA = record
  fTest: integer;
  function Test(aRecurse: Boolean): Integer; empty;
end;

TRecB = record helper for TRecA
  function Test(aRecurse: Boolean): Integer; empty;
  //property AccessTest: Integer read fTest;
end;


  JClassZ = partial class external 'Object'
    el: string;
    data: JObject;
    methods: JSValue;
    meth: TRecA;
    &method = record
      test: procedure; external "test";
    end;
    //upvoteClick: procedure();
  end;

  JClassUm = class external 'Object' (JClassZ)
      //upvoteClick: procedure();
    upvoteClick: procedure();
  end;

  function CreateObj: JClassUm; external '{}' property;
  function this: JSValue; external 'this' property;
  var opt external 'Self.opt': JClassUm;


//-----------------------------------------------

Type
  TObjectHelper = class helper for TObject
    function MemoryLocation: String; empty;
  end;


Type
  TAObjectHelper = class helper for TObject
    function MemoryLocation: String; empty;
  end;
//-----------------------------------------------
type
  JMyConsole = class external 'Console'
  public
    procedure log(data: JSValue); overload;
    procedure log(const data: array of const); overload;
  end;

var
  Con external 'console': JMyConsole;
*)

type
 TVueStaticMethods = class external name 'VueStaticMethods'
   function filter(definition: JSValue): JSValue; overload; external name 'filter';
   function filter(id: JSValue; definition: JSValue): JSValue; overload; external name 'filter';

   function component(id: JSValue; definition: JSValue): JSValue; overload; external name 'component';
   function use(plugin: JSValue): JSValue; overload; external name 'use';
end;

 //var window external "window" : JSValue;
// function this: JSValue; external 'this' property;
// function CreateObject: JSValue; external '{}' property;
// function CreateArray: JSValue; external '[]' property;
// function Vue: TVueStaticMethods; external 'Vue' property;
// function httpVueLoader: TVueStaticMethods; external 'httpVueLoader' property;


 //function filter(id: JSValue; definition: JSValue): JSValue; overload; external "Vue.filter";
 //function filter(definition: JSValue): JSValue; overload; external "Vue.filter";

 var
   CreateObject: JSValue; external name '{}';
   vueInstance: JVue;

implementation

end.

