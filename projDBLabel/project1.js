var pas = {};

var rtl = {

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist, implcode){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist+' hasimplcode='+rtl.isFunction(implcode));
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');
    if (!(implcode==undefined) && !rtl.isFunction(implcode)) rtl.error('invalid implementation code of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: implcode,
      $impl: null,
      $rtti: Object.create(rtl.tSectionRTTI)
    };
    module.$rtti.$module = module;
    if (implcode) module.$impl = {
      $module: module,
      $rtti: module.$rtti
    };
  },

  exitcode: 0,

  run: function(module_name){
  
    function doRun(){
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if (module_name=='program'){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas.program.$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    }
    
    if (rtl.showUncaughtExceptions) {
      try{
        doRun();
      } catch(re) {
        var errMsg = re.hasOwnProperty('$class') ? re.$class.$classname : '';
	    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
        alert('Uncaught Exception : '+errMsg);
        rtl.exitCode = 216;
      }
    } else {
      doRun();
    }
    return rtl.exitcode;
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    for (var i in useslist){
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  cloneCallback: function(cb){
    return rtl.createCallback(cb.scope,cb.fn);
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a==b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn==b.fn);
    }
  },

  initClass: function(c,parent,name,initfn){
    parent[name] = c;
    c.$classname = name;
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    c.$fullname = parent.$name+'.'+name;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.name+'.'+name;
    };
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$name,{ "class": c, module: parent });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = {};
      c.$create = function(fnname,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
        o.$init();
        try{
          o[fnname].apply(o,args);
          o.AfterConstruction();
        } catch($e){
          o.$destroy;
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        this[fnname]();
        this.$final;
      };
    };
    rtl.initClass(c,parent,name,initfn);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var c = null;
    c = Object.create(ancestor);
    c.$create = function(fnname,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fnname,args);
      } else {
        o = Object.create(this);
      }
      o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
      o.$init();
      try{
        o[fnname].apply(o,args);
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        o.$destroy;
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      this[fnname]();
      this.$final;
    };
    rtl.initClass(c,parent,name,initfn);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = name;
    i.$fullname = module.$name+'.'+name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,new TGuid());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    if (!aclass.hasOwnProperty('$intfmaps')) aclass.$intfmaps = {};
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.queryIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      this[id]=intf;
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          //console.log('rtl.intfRefs.free: id='+id+' '+this[id].$name+' $o='+this[id].$o.$classname);
          this[id]._Release();
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    // multi dim: (arr,defaultvalue,dim1,dim2,...)
    if (arr == null) arr = [];
    var p = arguments;
    function setLength(a,argNo){
      var oldlen = a.length;
      var newlen = p[argNo];
      if (oldlen!==newlength){
        a.length = newlength;
        if (argNo === p.length-1){
          if (rtl.isArray(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
          } else if (rtl.isFunction(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=new defaultvalue(); // e.g. record
          } else if (rtl.isObject(defaultvalue)) {
            for (var i=oldlen; i<newlen; i++) a[i]={}; // e.g. set
          } else {
            for (var i=oldlen; i<newlen; i++) a[i]=defaultvalue;
          }
        } else {
          for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
        }
      }
      if (argNo < p.length-1){
        // multi argNo
        for (var i=0; i<newlen; i++) a[i]=setLength(a[i],argNo+1);
      }
      return a;
    }
    return setLength(arr,2);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if (rtl.isFunction(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = new type(src[srcpos]); // clone record
    } else if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=src; // Note: concat(a) does not clone
      } else {
        a=a.concat(src);
      }
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    s.$shared = true;
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    delete r.$shared;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
    };
  },

  floatToStr : function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (ad<1.0e+10) {
		pad='00';
	  } else if (ad<1.0e+100) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=9;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo" };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = {};
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params);
      t.procsig.resulttype = result?result:null;
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      if (rtl.isArray(t.params)) t.params = rtl.newTIParams(t.params);
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",15 /* tkInterface */,rtl.tTypeInfoStruct);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result,
      flags: flags
    };
    return s;
  }
}
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  this.LineEnding = "\n";
  this.MaxLongint = 0x7fffffff;
  this.Maxint = 2147483647;
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
  });
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.Int = function (A) {
    var Result = 0.0;
    Result = Math.trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if (((Index < 1) || (Index > S.get().length)) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set(($mod.Copy(t,1,Index - 1) + Insertion) + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else NI.set($mod.Trunc(x));
  };
  this.val$5 = function (S, I, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else if (x > 2147483647) {
      Code.set(2)}
     else I.set($mod.Trunc(x));
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    Result = "";
    for (var $l1 = 1, $end2 = l; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + c;
    };
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SListCapacityError = "List capacity (%s) exceeded.";
  this.SListCountError = "List count (%s) out of bounds.";
  this.SListIndexError = "List index (%s) out of bounds";
  this.SInvalidName = 'Invalid component name: "%s"';
  this.SDuplicateName = 'Duplicate component name: "%s"';
  this.SErrInvalidInteger = 'Invalid integer value: "%s"';
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  this.TSize = function (s) {
    if (s) {
      this.cx = s.cx;
      this.cy = s.cy;
    } else {
      this.cx = 0;
      this.cy = 0;
    };
    this.$equal = function (b) {
      return (this.cx === b.cx) && (this.cy === b.cy);
    };
  };
  this.TPoint = function (s) {
    if (s) {
      this.x = s.x;
      this.y = s.y;
    } else {
      this.x = 0;
      this.y = 0;
    };
    this.$equal = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
  };
  $mod.$rtti.$Record("TPoint",{}).addFields("x",rtl.longint,"y",rtl.longint);
  this.TRect = function (s) {
    if (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
    } else {
      this.Left = 0;
      this.Top = 0;
      this.Right = 0;
      this.Bottom = 0;
    };
    this.$equal = function (b) {
      return (this.Left === b.Left) && ((this.Top === b.Top) && ((this.Right === b.Right) && (this.Bottom === b.Bottom)));
    };
  };
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = new $mod.TRect();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = new $mod.TPoint();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.Size = function (AWidth, AHeight) {
    var Result = new $mod.TSize();
    Result.cx = AWidth;
    Result.cy = AHeight;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  this.isBoolean = function (v) {
    return typeof(v) == 'boolean';
  };
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.isUndefined = function (v) {
    return v == undefined;
  };
  this.TJSValueType = {"0": "jvtNull", jvtNull: 0, "1": "jvtBoolean", jvtBoolean: 1, "2": "jvtInteger", jvtInteger: 2, "3": "jvtFloat", jvtFloat: 3, "4": "jvtString", jvtString: 4, "5": "jvtObject", jvtObject: 5, "6": "jvtArray", jvtArray: 6};
  this.GetValueType = function (JS) {
    var Result = 0;
    var t = "";
    if ($mod.isNull(JS)) {
      Result = 0}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = 4}
       else if (t === "boolean") {
        Result = 1}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = 6}
         else Result = 5;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = 2}
       else Result = 3;
    };
    return Result;
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  this.TFloatRec = function (s) {
    if (s) {
      this.Exponent = s.Exponent;
      this.Negative = s.Negative;
      this.Digits = s.Digits;
    } else {
      this.Exponent = 0;
      this.Negative = false;
      this.Digits = [];
    };
    this.$equal = function (b) {
      return (this.Exponent === b.Exponent) && ((this.Negative === b.Negative) && (this.Digits === b.Digits));
    };
  };
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
    };
    this.CreateFmt = function (Msg, Args) {
      this.fMessage = $mod.Format(Msg,Args);
    };
  });
  rtl.createClass($mod,"EAbort",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EVariantError",$mod.Exception,function () {
  });
  this.Trim = function (S) {
    return S.trim();
  };
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.AnsiSameText = function (s1, s2) {
    return s1.localeCompare(s2) == 0;
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while (((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9")) && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === -1) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          if (rtl.isNumber(Args[ArgN]) && pas.JS.isInteger(Args[ArgN])) {
            Value = Math.floor(Args[ArgN])}
           else $impl.DoFormatError(1,Fmt);
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = -1;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === -1) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (pas.JS.GetValueType(Args[DoArg]) !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp1 = Fchar;
        if ($tmp1 === "D") {
          Checkarg(2,true);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp1 === "U") {
          Checkarg(2,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),0,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),0,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),1,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),3,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),4,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(4,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(2,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(2,true);
          vq = Math.floor(Args[DoArg]);
          Index = 31;
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while (((1 << (Index * 4)) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp1 === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    var Radix = 10;
    var F = "";
    var N = "";
    var J = undefined;
    N = S;
    F = pas.System.Copy(N,1,1);
    if (F === "$") {
      Radix = 16}
     else if (F === "&") {
      Radix = 8}
     else if (F === "%") Radix = 2;
    if (Radix !== 10) pas.System.Delete({get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }},1,1);
    J = parseInt(N,Radix);
    Result = !isNaN(J);
    if (Result) res.set(Math.floor(J));
    return Result;
  };
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToInt = function (S) {
    var Result = 0;
    var R = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = R;
    return Result;
  };
  var HexDigits = "0123456789ABCDEF";
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    if (Digits === 0) Digits = 1;
    Result = "";
    while (Value > 0) {
      Result = HexDigits.charAt(((Value & 15) + 1) - 1) + Result;
      Value = Value >>> 4;
    };
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  var Rounds = "234567890";
  this.FloatToDecimal = function (Value, Precision, Decimals) {
    var Result = new $mod.TFloatRec();
    var Buffer = "";
    var InfNan = "";
    var error = 0;
    var N = 0;
    var L = 0;
    var Start = 0;
    var C = 0;
    var GotNonZeroBeforeDot = false;
    var BeforeDot = false;
    if (Value === 0) ;
    Result.Digits = rtl.arraySetLength(Result.Digits,"",19);
    Buffer=Value.toPrecision(21); // Double precision;
    N = 1;
    L = Buffer.length;
    while (Buffer.charAt(N - 1) === " ") N += 1;
    Result.Negative = Buffer.charAt(N - 1) === "-";
    if (Result.Negative) {
      N += 1}
     else if (Buffer.charAt(N - 1) === "+") N += 1;
    if (L >= (N + 2)) {
      InfNan = pas.System.Copy(Buffer,N,3);
      if (InfNan === "Inf") {
        Result.Digits[0] = "\x00";
        Result.Exponent = 32767;
        return Result;
      };
      if (InfNan === "Nan") {
        Result.Digits[0] = "\x00";
        Result.Exponent = -32768;
        return Result;
      };
    };
    Start = N;
    Result.Exponent = 0;
    BeforeDot = true;
    GotNonZeroBeforeDot = false;
    while ((L >= N) && (Buffer.charAt(N - 1) !== "E")) {
      if (Buffer.charAt(N - 1) === ".") {
        BeforeDot = false}
       else {
        if (BeforeDot) {
          Result.Exponent += 1;
          Result.Digits[N - Start] = Buffer.charAt(N - 1);
          if (Buffer.charAt(N - 1) !== "0") GotNonZeroBeforeDot = true;
        } else Result.Digits[(N - Start) - 1] = Buffer.charAt(N - 1);
      };
      N += 1;
    };
    N += 1;
    if (N <= L) {
      pas.System.val$5(pas.System.Copy(Buffer,N,(L - N) + 1),{get: function () {
          return C;
        }, set: function (v) {
          C = v;
        }},{get: function () {
          return error;
        }, set: function (v) {
          error = v;
        }});
      Result.Exponent += C;
    };
    if (BeforeDot) {
      N = (N - Start) - 1}
     else N = (N - Start) - 2;
    L = rtl.length(Result.Digits);
    if (N < L) Result.Digits[N] = "0";
    if ((Decimals + Result.Exponent) < Precision) {
      N = Decimals + Result.Exponent}
     else N = Precision;
    if (N >= L) N = L - 1;
    if (N === 0) {
      if (Result.Digits[0] >= "5") {
        Result.Digits[0] = "1";
        Result.Digits[1] = "\x00";
        Result.Exponent += 1;
      } else Result.Digits[0] = "\x00";
    } else if (N > 0) {
      if (Result.Digits[N] >= "5") {
        do {
          Result.Digits[N] = "\x00";
          N -= 1;
          Result.Digits[N] = Rounds.charAt($mod.StrToInt(Result.Digits[N]) - 1);
        } while (!((N === 0) || (Result.Digits[N] < ":")));
        if (Result.Digits[0] === ":") {
          Result.Digits[0] = "1";
          Result.Exponent += 1;
        };
      } else {
        Result.Digits[N] = "0";
        while ((N > -1) && (Result.Digits[N] === "0")) {
          Result.Digits[N] = "\x00";
          N -= 1;
        };
      };
    } else Result.Digits[0] = "\x00";
    if ((Result.Digits[0] === "\x00") && !GotNonZeroBeforeDot) {
      Result.Exponent = 0;
      Result.Negative = false;
    };
    return Result;
  };
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,1,15,0);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === 1) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === 2) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === 0) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === 3) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator)}
     else if ($tmp1 === 4) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,DS,$mod.ThousandSeparator);
    if (((format !== 4) && (Result.length > 1)) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    return Result;
  };
  var MaxPrecision = 18;
  this.FormatFloat = function (Fmt, aValue) {
    var Result = "";
    var E = 0.0;
    var FV = new $mod.TFloatRec();
    var Section = "";
    var SectionLength = 0;
    var ThousandSep = false;
    var IsScientific = false;
    var DecimalPos = 0;
    var FirstDigit = 0;
    var LastDigit = 0;
    var RequestedDigits = 0;
    var ExpSize = 0;
    var Available = 0;
    var Current = 0;
    var PadZeroes = 0;
    var DistToDecimal = 0;
    function InitVars() {
      E = aValue;
      Section = "";
      SectionLength = 0;
      ThousandSep = false;
      IsScientific = false;
      DecimalPos = 0;
      FirstDigit = 2147483647;
      LastDigit = 0;
      RequestedDigits = 0;
      ExpSize = 0;
      Available = -1;
    };
    function ToResult(AChar) {
      Result = Result + AChar;
    };
    function AddToResult(AStr) {
      Result = Result + AStr;
    };
    function WriteDigit(ADigit) {
      if (ADigit === "\x00") return;
      DistToDecimal -= 1;
      if (DistToDecimal === -1) {
        AddToResult($mod.DecimalSeparator);
        ToResult(ADigit);
      } else {
        ToResult(ADigit);
        if ((ThousandSep && ((DistToDecimal % 3) === 0)) && (DistToDecimal > 1)) AddToResult($mod.ThousandSeparator);
      };
    };
    function GetDigit() {
      var Result = "";
      Result = "\x00";
      if (Current <= Available) {
        Result = FV.Digits[Current];
        Current += 1;
      } else if (DistToDecimal <= LastDigit) {
        DistToDecimal -= 1}
       else Result = "0";
      return Result;
    };
    function CopyDigit() {
      if (PadZeroes === 0) {
        WriteDigit(GetDigit())}
       else if (PadZeroes < 0) {
        PadZeroes += 1;
        if (DistToDecimal <= FirstDigit) {
          WriteDigit("0")}
         else DistToDecimal -= 1;
      } else {
        while (PadZeroes > 0) {
          WriteDigit(GetDigit());
          PadZeroes -= 1;
        };
        WriteDigit(GetDigit());
      };
    };
    function GetSections(SP) {
      var Result = 0;
      var FL = 0;
      var i = 0;
      var C = "";
      var Q = "";
      var inQuote = false;
      Result = 1;
      SP.get()[1] = -1;
      SP.get()[2] = -1;
      SP.get()[3] = -1;
      inQuote = false;
      Q = "\x00";
      i = 1;
      FL = Fmt.length;
      while (i <= FL) {
        C = Fmt.charAt(i - 1);
        var $tmp1 = C;
        if ($tmp1 === ";") {
          if (!inQuote) {
            if (Result > 3) throw $mod.Exception.$create("Create$1",["Invalid float format"]);
            SP.get()[Result] = i + 1;
            Result += 1;
          };
        } else if (($tmp1 === '"') || ($tmp1 === "'")) {
          if (inQuote) {
            inQuote = C !== Q}
           else {
            inQuote = true;
            Q = C;
          };
        };
        i += 1;
      };
      if (SP.get()[Result] === -1) SP.get()[Result] = FL + 1;
      return Result;
    };
    function AnalyzeFormat() {
      var I = 0;
      var Len = 0;
      var Q = "";
      var C = "";
      var InQuote = false;
      Len = Section.length;
      I = 1;
      InQuote = false;
      Q = "\x00";
      while (I <= Len) {
        C = Section.charAt(I - 1);
        if (C.charCodeAt() in rtl.createSet(34,39)) {
          if (InQuote) {
            InQuote = C !== Q}
           else {
            InQuote = true;
            Q = C;
          };
        } else if (!InQuote) {
          var $tmp1 = C;
          if ($tmp1 === ".") {
            if (DecimalPos === 0) DecimalPos = RequestedDigits + 1}
           else if ($tmp1 === ",") {
            ThousandSep = $mod.ThousandSeparator !== "\x00"}
           else if (($tmp1 === "e") || ($tmp1 === "E")) {
            I += 1;
            if (I < Len) {
              C = Section.charAt(I - 1);
              IsScientific = C.charCodeAt() in rtl.createSet(45,43);
              if (IsScientific) while ((I < Len) && (Section.charAt((I + 1) - 1) === "0")) {
                ExpSize += 1;
                I += 1;
              };
              if (ExpSize > 4) ExpSize = 4;
            };
          } else if ($tmp1 === "#") {
            RequestedDigits += 1}
           else if ($tmp1 === "0") {
            if (RequestedDigits < FirstDigit) FirstDigit = RequestedDigits + 1;
            RequestedDigits += 1;
            LastDigit = RequestedDigits + 1;
          };
        };
        I += 1;
      };
      if (DecimalPos === 0) DecimalPos = RequestedDigits + 1;
      LastDigit = DecimalPos - LastDigit;
      if (LastDigit > 0) LastDigit = 0;
      FirstDigit = DecimalPos - FirstDigit;
      if (FirstDigit < 0) FirstDigit = 0;
    };
    function ValueOutSideScope() {
      var Result = false;
      Result = (((FV.Exponent >= 18) && !IsScientific) || (FV.Exponent === 0x7FF)) || (FV.Exponent === 0x800);
      return Result;
    };
    function CalcRunVars() {
      var D = 0;
      var P = 0;
      if (IsScientific) {
        P = RequestedDigits;
        D = 9999;
      } else {
        P = 18;
        D = (RequestedDigits - DecimalPos) + 1;
      };
      FV = new $mod.TFloatRec($mod.FloatToDecimal(aValue,P,D));
      DistToDecimal = DecimalPos - 1;
      if (IsScientific) {
        PadZeroes = 0}
       else {
        PadZeroes = FV.Exponent - (DecimalPos - 1);
        if (PadZeroes >= 0) DistToDecimal = FV.Exponent;
      };
      Available = -1;
      while ((Available < (rtl.length(FV.Digits) - 1)) && (FV.Digits[Available + 1] !== "\x00")) Available += 1;
    };
    function FormatExponent(ASign, aExponent) {
      var Result = "";
      Result = $mod.IntToStr(aExponent);
      Result = pas.System.StringOfChar("0",ExpSize - Result.length) + Result;
      if (aExponent < 0) {
        Result = "-" + Result}
       else if ((aExponent > 0) && (ASign === "+")) Result = ASign + Result;
      return Result;
    };
    var I = 0;
    var S = 0;
    var C = "";
    var Q = "";
    var PA = [];
    var InLiteral = false;
    PA = rtl.arraySetLength(PA,0,4);
    Result = "";
    InitVars();
    if (E > 0) {
      S = 1}
     else if (E < 0) {
      S = 2}
     else S = 3;
    PA[0] = 0;
    I = GetSections({get: function () {
        return PA;
      }, set: function (v) {
        PA = v;
      }});
    if ((I < S) || ((PA[S] - PA[S - 1]) === 0)) S = 1;
    SectionLength = (PA[S] - PA[S - 1]) - 1;
    Section = pas.System.Copy(Fmt,PA[S - 1] + 1,SectionLength);
    Section = rtl.strSetLength(Section,SectionLength);
    AnalyzeFormat();
    CalcRunVars();
    if ((SectionLength === 0) || ValueOutSideScope()) {
      Section=E.toPrecision(15);
      Result = Section;
    };
    I = 1;
    Current = 0;
    Q = " ";
    InLiteral = false;
    if (FV.Negative && (S === 1)) ToResult("-");
    while (I <= SectionLength) {
      C = Section.charAt(I - 1);
      if (C.charCodeAt() in rtl.createSet(34,39)) {
        if (InLiteral) {
          InLiteral = C !== Q}
         else {
          InLiteral = true;
          Q = C;
        };
      } else if (InLiteral) {
        ToResult(C)}
       else {
        var $tmp1 = C;
        if (($tmp1 === "0") || ($tmp1 === "#")) {
          CopyDigit()}
         else if (($tmp1 === ".") || ($tmp1 === ",")) {}
        else if (($tmp1 === "e") || ($tmp1 === "E")) {
          ToResult(C);
          I += 1;
          if (I <= Section.length) {
            C = Section.charAt(I - 1);
            if (C.charCodeAt() in rtl.createSet(43,45)) {
              AddToResult(FormatExponent(C,(FV.Exponent - DecimalPos) + 1));
              while ((I < SectionLength) && (Section.charAt((I + 1) - 1) === "0")) I += 1;
            };
          };
        } else {
          ToResult(C);
        };
      };
      I += 1;
    };
    return Result;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = "Application raised an exception " + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = (S + " : ") + ExceptObject.fMessage;
    window.alert(S);
    if (ExceptAddr === null) ;
  };
  this.Abort = function () {
    throw $mod.EAbort.$create("Create$1",[$impl.SAbortError]);
  };
  this.TTimeStamp = function (s) {
    if (s) {
      this.Time = s.Time;
      this.Date = s.Date;
    } else {
      this.Time = 0;
      this.Date = 0;
    };
    this.$equal = function (b) {
      return (this.Time === b.Time) && (this.Date === b.Date);
    };
  };
  this.TimeSeparator = ":";
  this.DateSeparator = "-";
  this.ShortDateFormat = "yyyy-mm-dd";
  this.LongDateFormat = "ddd, yyyy-mm-dd";
  this.ShortTimeFormat = "hh:nn";
  this.LongTimeFormat = "hh:nn:ss";
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  this.TimeAMString = "AM";
  this.TimePMString = "PM";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.DateDelta = 693594;
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  this.LongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
  this.ShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
  this.LongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
  });
  this.FormatSettings = null;
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = new $mod.TTimeStamp();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = pas.System.Trunc(Math.abs(pas.System.Trunc(D)) % 86400000);
    Result.Date = 693594 + Math.floor(pas.System.Trunc(D) / 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (((((Year > 0) && (Year < 10000)) && (Month >= 1)) && (Month <= 12)) && (Day > 0)) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = Math.floor(Year / 100);
      ya = Year - (100 * c);
      date.set(((((146097 * c) >>> 2) + ((1461 * ya) >>> 2)) + Math.floor(((153 * Month) + 2) / 5)) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (((Hour < 24) && (Min < 60)) && (Sec < 60)) && (MSec < 1000);
    if (Result) Time.set(((((Hour * 3600000) + (Min * 60000)) + (Sec * 1000)) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s-%s-%s is not a valid date specification",[$mod.IntToStr(Year),$mod.IntToStr(Month),$mod.IntToStr(Day)]]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",[$mod.IntToStr(Hour),$mod.IntToStr(Minute),$mod.IntToStr(Second),$mod.IntToStr(MilliSecond)]]);
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = ((pas.System.Trunc(date) + 693900) << 2) - 1;
      ly = Math.floor(j / 146097);
      j = j - (146097 * ly);
      ld = j >>> 2;
      j = Math.floor(((ld << 2) + 3) / 1461);
      ld = (((ld << 2) + 7) - (1461 * j)) >>> 2;
      lm = Math.floor(((5 * ld) - 3) / 153);
      ld = Math.floor((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(Math.floor(l / 3600000));
    l = l % 3600000;
    Minute.set(Math.floor(l / 60000));
    l = l % 60000;
    Second.set(Math.floor(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + ((pas.System.Trunc(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + pas.System.Copy(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp1 === "A") || ($tmp1 === "a")) {
          if ((($mod.CompareText(pas.System.Copy(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,4),"AMPM") === 0)) || ($mod.CompareText(pas.System.Copy(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp2 = Token;
        if (($tmp2 === "'") || ($tmp2 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp2 === "A") {
          if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString($mod.TimeAMString)}
             else StoreString($mod.TimePMString);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $mod.EConvertError.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp2 === "\/") {
          StoreString($mod.DateSeparator);
        } else if ($tmp2 === ":") {
          StoreString($mod.TimeSeparator)}
         else if ((((((((((($tmp2 === " ") || ($tmp2 === "C")) || ($tmp2 === "D")) || ($tmp2 === "H")) || ($tmp2 === "M")) || ($tmp2 === "N")) || ($tmp2 === "S")) || ($tmp2 === "T")) || ($tmp2 === "Y")) || ($tmp2 === "Z")) || ($tmp2 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp3 = Token;
          if ($tmp3 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp3 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp3 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp4 = Count;
              if ($tmp4 === 1) {
                StoreInt(Month,0)}
               else if ($tmp4 === 2) {
                StoreInt(Month,2)}
               else if ($tmp4 === 3) {
                StoreString($mod.ShortMonthNames[Month - 1])}
               else {
                StoreString($mod.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp3 === "D") {
            var $tmp5 = Count;
            if ($tmp5 === 1) {
              StoreInt(Day,0)}
             else if ($tmp5 === 2) {
              StoreInt(Day,2)}
             else if ($tmp5 === 3) {
              StoreString($mod.ShortDayNames[DayOfWeek])}
             else if ($tmp5 === 4) {
              StoreString($mod.LongDayNames[DayOfWeek])}
             else if ($tmp5 === 5) {
              StoreFormat($mod.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat($mod.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp3 === "H") {
            if (isInterval) {
              StoreInt(Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp3 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp3 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp3 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp3 === "T") {
            if (Count === 1) {
              StoreFormat($mod.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat($mod.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp3 === "C") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            if (((Hour !== 0) || (Minute !== 0)) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp3 === "F") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 2;
  this.CurrencyString = "$";
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.SAbortError = "Operation aborted";
  $impl.feInvalidFormat = 1;
  $impl.feMissingArgument = 2;
  $impl.feInvalidArgIndex = 3;
  $impl.DoFormatError = function (ErrCode, fmt) {
    var $tmp1 = ErrCode;
    if ($tmp1 === 1) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidFormat,[fmt]])}
     else if ($tmp1 === 2) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SArgumentMissing,[fmt]])}
     else if ($tmp1 === 3) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidArgIndex,[fmt]]);
  };
  $impl.maxdigits = 15;
  $impl.ReplaceDecimalSep = function (S, DS) {
    var Result = "";
    var P = 0;
    P = pas.System.Pos(".",S);
    if (P > 0) {
      Result = (pas.System.Copy(S,1,P - 1) + DS) + pas.System.Copy(S,P + 1,S.length - P)}
     else Result = S;
    return Result;
  };
  $impl.FormatGeneralFloat = function (Value, Precision, DS) {
    var Result = "";
    var P = 0;
    var PE = 0;
    var Q = 0;
    var Exponent = 0;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    Result = $mod.TrimLeft(Result);
    P = pas.System.Pos(".",Result);
    if (P === 0) return Result;
    PE = pas.System.Pos("E",Result);
    if (PE === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    Q = PE + 2;
    Exponent = 0;
    while (Q <= Result.length) {
      Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - "0".charCodeAt();
      Q += 1;
    };
    if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
    if (((P + Exponent) < PE) && (Exponent > -6)) {
      Result = rtl.strSetLength(Result,PE - 1);
      if (Exponent >= 0) {
        for (var $l1 = 0, $end2 = Exponent - 1; $l1 <= $end2; $l1++) {
          Q = $l1;
          Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
          P += 1;
        };
        Result = rtl.setCharAt(Result,P - 1,".");
        P = 1;
        if (Result.charAt(P - 1) === "-") P += 1;
        while (((Result.charAt(P - 1) === "0") && (P < Result.length)) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
      } else {
        pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 1);
        Result = rtl.setCharAt(Result,(P - Exponent) - 1,Result.charAt(((P - Exponent) - 1) - 1));
        Result = rtl.setCharAt(Result,P - 1,".");
        if (Exponent !== -1) Result = rtl.setCharAt(Result,((P - Exponent) - 1) - 1,"0");
      };
      Q = Result.length;
      while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
      if (Result.charAt(Q - 1) === ".") Q -= 1;
      if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
        Result = "0"}
       else Result = rtl.strSetLength(Result,Q);
    } else {
      while (Result.charAt((PE - 1) - 1) === "0") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE - 1) - 1) === DS) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE + 1) - 1) === "+") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1)}
       else PE += 1;
      while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},PE + 1,1);
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
    var Result = "";
    var P = 0;
    DS = $mod.DecimalSeparator;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos("E",Result);
    if (P === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    P += 2;
    if (Digits > 4) Digits = 4;
    Digits = ((Result.length - P) - Digits) + 1;
    if (Digits < 0) {
      pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P)}
     else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
      pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P,1);
      if (P > Result.length) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 2,2);
        break;
      };
      Digits -= 1;
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatFixedFloat = function (Value, Digits, DS) {
    var Result = "";
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
    var Result = "";
    var P = 0;
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 15) Digits = 15;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    P -= 3;
    if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
      if (Result.charAt((P - 1) - 1) !== "-") pas.System.Insert(TS,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    return Result;
  };
  $impl.RemoveLeadingNegativeSign = function (AValue, DS) {
    var Result = false;
    var i = 0;
    var TS = "";
    var StartPos = 0;
    Result = false;
    StartPos = 2;
    TS = $mod.ThousandSeparator;
    for (var $l1 = StartPos, $end2 = AValue.get().length; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get() === TS);
      if (!Result) break;
    };
    if (Result) pas.System.Delete(AValue,1,1);
    return Result;
  };
  $impl.FormatNumberCurrency = function (Value, Digits, DS, TS) {
    var Result = "";
    var Negative = false;
    var P = 0;
    if (Digits === -1) {
      Digits = $mod.CurrencyDecimals}
     else if (Digits > 18) Digits = 18;
    Result = rtl.spaceLeft("" + Value,0);
    Negative = Result.charAt(0) === "-";
    if (Negative) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    if (P !== 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS)}
     else P = Result.length + 1;
    P -= 3;
    while (P > 1) {
      if ($mod.ThousandSeparator !== "\x00") pas.System.Insert($mod.FormatSettings.GetThousandSeparator(),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    if ((Result.length > 1) && Negative) Negative = !$impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    if (!Negative) {
      var $tmp1 = $mod.CurrencyFormat;
      if ($tmp1 === 0) {
        Result = $mod.CurrencyString + Result}
       else if ($tmp1 === 1) {
        Result = Result + $mod.CurrencyString}
       else if ($tmp1 === 2) {
        Result = ($mod.CurrencyString + " ") + Result}
       else if ($tmp1 === 3) Result = (Result + " ") + $mod.CurrencyString;
    } else {
      var $tmp2 = $mod.NegCurrFormat;
      if ($tmp2 === 0) {
        Result = (("(" + $mod.CurrencyString) + Result) + ")"}
       else if ($tmp2 === 1) {
        Result = ("-" + $mod.CurrencyString) + Result}
       else if ($tmp2 === 2) {
        Result = ($mod.CurrencyString + "-") + Result}
       else if ($tmp2 === 3) {
        Result = ($mod.CurrencyString + Result) + "-"}
       else if ($tmp2 === 4) {
        Result = (("(" + Result) + $mod.CurrencyString) + ")"}
       else if ($tmp2 === 5) {
        Result = ("-" + Result) + $mod.CurrencyString}
       else if ($tmp2 === 6) {
        Result = (Result + "-") + $mod.CurrencyString}
       else if ($tmp2 === 7) {
        Result = (Result + $mod.CurrencyString) + "-"}
       else if ($tmp2 === 8) {
        Result = (("-" + Result) + " ") + $mod.CurrencyString}
       else if ($tmp2 === 9) {
        Result = (("-" + $mod.CurrencyString) + " ") + Result}
       else if ($tmp2 === 10) {
        Result = ((Result + " ") + $mod.CurrencyString) + "-"}
       else if ($tmp2 === 11) {
        Result = (($mod.CurrencyString + " ") + Result) + "-"}
       else if ($tmp2 === 12) {
        Result = (($mod.CurrencyString + " ") + "-") + Result}
       else if ($tmp2 === 13) {
        Result = ((Result + "-") + " ") + $mod.CurrencyString}
       else if ($tmp2 === 14) {
        Result = ((("(" + $mod.CurrencyString) + " ") + Result) + ")"}
       else if ($tmp2 === 15) Result = ((("(" + Result) + " ") + $mod.CurrencyString) + ")";
    };
    if (TS === "") ;
    return Result;
  };
});
rtl.module("Classes",["System","RTLConsts","Types","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass($mod,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass($mod,"EComponentError",pas.SysUtils.Exception,function () {
  });
  this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  $mod.$rtti.$Enum("TAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass($mod,"TFPList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.Put = function (Index, Item) {
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      this.FList[Index] = Item;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.Error(pas.RTLConsts.SListCapacityError,"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.Error(pas.RTLConsts.SListCountError,"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.Error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,[Data]]);
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === 0) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass($mod,"TList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Notify = function (aValue, Action) {
      if (pas.System.Assigned(aValue)) ;
      if (Action === 1) ;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FList = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,0);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if (pas.System.Assigned(V)) this.Notify(V,2);
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      Result = this.FList.IndexOf(Item);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  rtl.createClass($mod,"TPersistent",pas.System.TObject,function () {
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw pas.SysUtils.EConvertError.$create("Create$1",[((("Cannot assign a " + SourceName) + " to a ") + this.$classname) + "."]);
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass($mod,"TCollectionItem",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FCollection !== null) {
        Result = this.FCollection.FItems.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection !== null) this.FCollection.RemoveItem(this);
        if (Value !== null) Value.InsertItem(this);
      };
    };
    this.Changed = function (AllItems) {
      if ((this.FCollection !== null) && (this.FCollection.FUpdateCount === 0)) {
        if (AllItems) {
          this.FCollection.Update(null)}
         else this.FCollection.Update(this);
      };
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FCollection;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      this.Changed(false);
      if (Value === "") ;
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.SetCollection(ACollection);
    };
    this.Destroy = function () {
      this.SetCollection(null);
      pas.System.TObject.Destroy.call(this);
    };
  });
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass($mod,"TCollection",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,0);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,1);
      I = this.FItems.IndexOfItem(Item,1);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === 0) ;
    };
    this.Create$1 = function (AItemClass) {
      pas.System.TObject.Create.call(this);
      this.FItemClass = AItemClass;
      this.FItems = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Owner = function () {
      var Result = null;
      Result = this.GetOwner();
      return Result;
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($mod.TCollection.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l1 = 0, $end2 = Source.GetCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
  });
  rtl.createClass($mod,"TOwnedCollection",$mod.TCollection,function () {
    this.$init = function () {
      $mod.TCollection.$init.call(this);
      this.FOwner = null;
    };
    this.$final = function () {
      this.FOwner = undefined;
      $mod.TCollection.$final.call(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Create$2 = function (AOwner, AItemClass) {
      this.FOwner = AOwner;
      $mod.TCollection.Create$1.call(this,AItemClass);
    };
  });
  this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass($mod,"TComponent",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $mod.TFPList.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,8);
        };
      };
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Loaded = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,0);
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === 1) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,4)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,4);
      if ((this.FComponents != null) && SetChildren) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !pas.SysUtils.IsValidIdent(NewName,false,false)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SInvalidName,[NewName]]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.ChangeName(NewName);
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((((AComponent !== null) && (pas.SysUtils.CompareText(CurName,NewName) !== 0)) && (AComponent.FOwner === this)) && (this.FindComponent(NewName) !== null)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SDuplicateName,[NewName]]);
      if ((4 in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet(0);
      if (AOwner != null) AOwner.InsertComponent(this);
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,1);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      pas.System.TObject.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!(3 in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if (3 in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,3);
      if (this.FComponents != null) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (pas.SysUtils.CompareText(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.FreeNotification = function (AComponent) {
      if ((this.FOwner !== null) && (AComponent === this.FOwner)) return;
      if (!(this.FFreeNotifies != null)) this.FFreeNotifies = $mod.TFPList.$create("Create");
      if (this.FFreeNotifies.IndexOf(AComponent) === -1) {
        this.FFreeNotifies.Add(AComponent);
        AComponent.FreeNotification(this);
      };
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      this.Insert(AComponent);
      if (4 in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,0);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,1);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag");
  });
  $mod.$init = function () {
    $impl.ClassList = Object.create(null);
  };
},["JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ClassList = null;
});
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Graphics",["System","Classes","SysUtils","Types","Web"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$Int("TFontCharSet",{minvalue: 0, maxvalue: 255, ordtype: 3});
  this.TFontStyle = {"0": "fsBold", fsBold: 0, "1": "fsItalic", fsItalic: 1, "2": "fsUnderline", fsUnderline: 2, "3": "fsStrikeOut", fsStrikeOut: 3};
  $mod.$rtti.$Enum("TFontStyle",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFontStyle});
  $mod.$rtti.$Set("TFontStyles",{comptype: $mod.$rtti["TFontStyle"]});
  this.TTextLayout = {"0": "tlTop", tlTop: 0, "1": "tlCenter", tlCenter: 1, "2": "tlBottom", tlBottom: 2};
  $mod.$rtti.$Enum("TTextLayout",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TTextLayout});
  this.TPenStyle = {"0": "psSolid", psSolid: 0, "1": "psDash", psDash: 1, "2": "psDot", psDot: 2, "3": "psDashDot", psDashDot: 3, "4": "psDashDotDot", psDashDotDot: 4, "5": "psInsideFrame", psInsideFrame: 5, "6": "psPattern", psPattern: 6, "7": "psClear", psClear: 7};
  $mod.$rtti.$Enum("TPenStyle",{minvalue: 0, maxvalue: 7, ordtype: 1, enumtype: this.TPenStyle});
  this.TBrushStyle = {"0": "bsSolid", bsSolid: 0, "1": "bsClear", bsClear: 1, "2": "bsHorizontal", bsHorizontal: 2, "3": "bsVertical", bsVertical: 3, "4": "bsFDiagonal", bsFDiagonal: 4, "5": "bsBDiagonal", bsBDiagonal: 5, "6": "bsCross", bsCross: 6, "7": "bsDiagCross", bsDiagCross: 7, "8": "bsImage", bsImage: 8, "9": "bsPattern", bsPattern: 9};
  $mod.$rtti.$Enum("TBrushStyle",{minvalue: 0, maxvalue: 9, ordtype: 1, enumtype: this.TBrushStyle});
  rtl.createClass($mod,"TFont",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FCharSet = 0;
      this.FColor = 0;
      this.FName = "";
      this.FSize = 0;
      this.FStyle = {};
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FStyle = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = Math.round((this.FSize * 96) / 72);
      return Result;
    };
    this.SetCharSet = function (AValue) {
      if (this.FCharSet !== AValue) {
        this.FCharSet = AValue;
        this.Changed();
      };
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetSize(Math.round((AValue * 72) / 96));
    };
    this.SetName = function (AValue) {
      if (this.FName !== AValue) {
        this.FName = AValue;
        this.Changed();
      };
    };
    this.SetSize = function (AValue) {
      if (this.FSize !== AValue) {
        this.FSize = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (rtl.neSet(this.FStyle,AValue)) {
        this.FStyle = rtl.refSet(AValue);
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FColor = 0;
      this.FName = $mod.ffMonospace;
      this.FSize = 16;
      this.FStyle = {};
      this.FUpdateCount = 0;
    };
    this.Assign = function (Source) {
      var VFont = null;
      if ((Source != null) && $mod.TFont.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VFont = Source;
          this.FCharSet = VFont.FCharSet;
          this.FColor = VFont.FColor;
          this.FName = VFont.FName;
          this.FSize = VFont.FSize;
          this.FStyle = rtl.refSet(VFont.FStyle);
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.IsEqual = function (AFont) {
      var Result = false;
      if (AFont != null) {
        if (((((this.FCharSet !== AFont.FCharSet) || (this.FColor !== AFont.FColor)) || (this.FName !== AFont.FName)) || (this.FSize !== AFont.FSize)) || rtl.neSet(this.FStyle,AFont.FStyle)) {
          Result = false;
        } else {
          Result = true;
        };
      } else {
        Result = false;
      };
      return Result;
    };
    this.TextExtent = function (AText) {
      var Result = new pas.Types.TSize();
      Result = new pas.Types.TSize($mod.JSMeasureText(AText,this.FName,this.FSize,0));
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("CharSet",2,$mod.$rtti["TFontCharSet"],"FCharSet","SetCharSet");
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Height",3,rtl.nativeint,"GetHeight","SetHeight");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Size",2,rtl.nativeint,"FSize","SetSize");
    $r.addProperty("Style",2,$mod.$rtti["TFontStyles"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass($mod,"TPen",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FWidth = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle !== AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.SetWidth = function (AValue) {
      if (this.FWidth !== AValue) {
        this.FWidth = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Assign = function (Source) {
      var VPen = null;
      if ((Source != null) && $mod.TPen.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VPen = Source;
          this.FColor = VPen.FColor;
          this.FStyle = VPen.FStyle;
          this.FWidth = VPen.FWidth;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TPenStyle"],"FStyle","SetStyle");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass($mod,"TBrush",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle === AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Assign = function (Source) {
      var VBrush = null;
      if ((Source != null) && $mod.TBrush.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VBrush = Source;
          this.FColor = VBrush.FColor;
          this.FStyle = VBrush.FStyle;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TBrushStyle"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass($mod,"TCanvas",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FBrush = undefined;
      this.FFont = undefined;
      this.FPen = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.Destroy = function () {
      this.FBrush.$destroy("Destroy");
      this.FFont.$destroy("Destroy");
      this.FPen.$destroy("Destroy");
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      pas.System.TObject.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Brush",0,$mod.$rtti["TBrush"],"FBrush","FBrush");
    $r.addProperty("Font",0,$mod.$rtti["TFont"],"FFont","FFont");
    $r.addProperty("Pen",0,$mod.$rtti["TPen"],"FPen","FPen");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  this.clBlack = 0x0;
  this.clWhite = 0xFFFFFF;
  this.clNone = 0x1FFFFFFF;
  this.clDefault = 0x20000000;
  this.clBase = 0x80000000;
  this.clScrollBar = 2147483648 + 0;
  this.clBackground = 2147483648 + 1;
  this.clActiveCaption = 2147483648 + 2;
  this.clInactiveCaption = 2147483648 + 3;
  this.clMenu = 2147483648 + 4;
  this.clWindow = 2147483648 + 5;
  this.clWindowFrame = 2147483648 + 6;
  this.clMenuText = 2147483648 + 7;
  this.clWindowText = 2147483648 + 8;
  this.clCaptionText = 2147483648 + 9;
  this.clActiveBorder = 2147483648 + 10;
  this.clInactiveBorder = 2147483648 + 11;
  this.clAppWorkspace = 2147483648 + 12;
  this.clHighlight = 2147483648 + 13;
  this.clHighlightText = 2147483648 + 14;
  this.clBtnFace = 2147483648 + 15;
  this.clBtnShadow = 2147483648 + 16;
  this.clGrayText = 2147483648 + 17;
  this.clBtnText = 2147483648 + 18;
  this.clInactiveCaptionText = 2147483648 + 19;
  this.clBtnHighlight = 2147483648 + 20;
  this.cl3DDkShadow = 2147483648 + 21;
  this.cl3DLight = 2147483648 + 22;
  this.clInfoText = 2147483648 + 23;
  this.clInfoBk = 2147483648 + 24;
  this.ffMonospace = "Consolas, monaco, monospace";
  this.JSColor = function (AColor) {
    var Result = "";
    var R = 0;
    var G = 0;
    var B = 0;
    var $tmp1 = AColor;
    if ($tmp1 === 2147483648) {
      Result = "Scrollbar"}
     else if ($tmp1 === 2147483649) {
      Result = "Background"}
     else if ($tmp1 === 2147483650) {
      Result = "ActiveCaption"}
     else if ($tmp1 === 2147483651) {
      Result = "InactiveCaption"}
     else if ($tmp1 === 2147483652) {
      Result = "Menu"}
     else if ($tmp1 === 2147483653) {
      Result = "Window"}
     else if ($tmp1 === 2147483654) {
      Result = "WindowFrame"}
     else if ($tmp1 === 2147483655) {
      Result = "MenuText"}
     else if ($tmp1 === 2147483656) {
      Result = "WindowText"}
     else if ($tmp1 === 2147483657) {
      Result = "CaptionText"}
     else if ($tmp1 === 2147483658) {
      Result = "ActiveBorder"}
     else if ($tmp1 === 2147483659) {
      Result = "InactiveBorder"}
     else if ($tmp1 === 2147483660) {
      Result = "AppWorkspace"}
     else if ($tmp1 === 2147483661) {
      Result = "Highlight"}
     else if ($tmp1 === 2147483662) {
      Result = "HighlightText"}
     else if ($tmp1 === 2147483663) {
      Result = "ButtonFace"}
     else if ($tmp1 === 2147483664) {
      Result = "ButtonShadow"}
     else if ($tmp1 === 2147483665) {
      Result = "GrayText"}
     else if ($tmp1 === 2147483666) {
      Result = "ButtonText"}
     else if ($tmp1 === 2147483667) {
      Result = "InactiveCaptionText"}
     else if ($tmp1 === 2147483668) {
      Result = "ButtonHighlight"}
     else if ($tmp1 === 2147483669) {
      Result = "ThreeDDarkShadow"}
     else if ($tmp1 === 2147483670) {
      Result = "ThreeDHighlight"}
     else if ($tmp1 === 2147483671) {
      Result = "InfoText"}
     else if ($tmp1 === 2147483672) {
      Result = "InfoBackground"}
     else {
      R = AColor & 0xFF;
      G = (AColor >>> 8) & 0xFF;
      B = (AColor >>> 16) & 0xFF;
      Result = (("#" + pas.SysUtils.IntToHex(R,2)) + pas.SysUtils.IntToHex(G,2)) + pas.SysUtils.IntToHex(B,2);
    };
    return Result;
  };
  this.JSMeasureText = function (AText, AFontName, AFontSize, AFixedWidth) {
    var Result = new pas.Types.TSize();
    var VDiv = null;
    Result = new pas.Types.TSize(pas.Types.Size(0,0));
    if (AText !== "") {
      VDiv = document.createElement("div");
      VDiv.style.setProperty("font-family",AFontName);
      VDiv.style.setProperty("font-size",pas.SysUtils.IntToStr(AFontSize) + "px");
      VDiv.style.setProperty("overflow","scroll");
      if (AFixedWidth === 0) {
        VDiv.style.setProperty("display","inline-block");
        VDiv.style.setProperty("white-space","nowrap");
      } else {
        VDiv.style.setProperty("max-width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
        VDiv.style.setProperty("width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
      };
      VDiv.innerHTML = AText;
      document.body.appendChild(VDiv);
      Result = new pas.Types.TSize(pas.Types.Size(VDiv.scrollWidth,VDiv.scrollHeight));
      document.body.removeChild(VDiv);
    };
    return Result;
  };
});
rtl.module("Controls",["System","Classes","SysUtils","Types","JS","Web","Graphics"],function () {
  "use strict";
  var $mod = this;
  this.mrNone = 0;
  this.crDefault = 0;
  this.crNone = -1;
  this.crCross = -3;
  this.crIBeam = -4;
  this.crSize = -22;
  this.crSizeNESW = -6;
  this.crSizeNS = -7;
  this.crSizeNWSE = -8;
  this.crSizeWE = -9;
  this.crSizeNW = -23;
  this.crSizeN = -24;
  this.crSizeNE = -25;
  this.crSizeW = -26;
  this.crSizeE = -27;
  this.crSizeSW = -28;
  this.crSizeS = -29;
  this.crSizeSE = -30;
  this.crHourGlass = -11;
  this.crNoDrop = -13;
  this.crHSplit = -14;
  this.crVSplit = -15;
  this.crSQLWait = -17;
  this.crNo = -18;
  this.crAppStart = -19;
  this.crHelp = -20;
  this.crHandPoint = -21;
  $mod.$rtti.$Class("TWinControl");
  this.TAlign = {"0": "alNone", alNone: 0, "1": "alTop", alTop: 1, "2": "alBottom", alBottom: 2, "3": "alLeft", alLeft: 3, "4": "alRight", alRight: 4, "5": "alClient", alClient: 5, "6": "alCustomd", alCustomd: 6};
  $mod.$rtti.$Enum("TAlign",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TAlign});
  this.TFormBorderStyle = {"0": "bsNone", bsNone: 0, "1": "bsSingle", bsSingle: 1, "2": "bsSizeable", bsSizeable: 2, "3": "bsDialog", bsDialog: 3, "4": "bsToolWindow", bsToolWindow: 4, "5": "bsSizeToolWin", bsSizeToolWin: 5};
  $mod.$rtti.$inherited("TCaption",rtl.string,{});
  $mod.$rtti.$Int("TCursor",{minvalue: -32768, maxvalue: 32767, ordtype: 2});
  rtl.createClass($mod,"TControlCanvas",pas.Graphics.TCanvas,function () {
  });
  this.TShiftStateEnum = {"0": "ssShift", ssShift: 0, "1": "ssAlt", ssAlt: 1, "2": "ssCtrl", ssCtrl: 2, "3": "ssLeft", ssLeft: 3, "4": "ssRight", ssRight: 4, "5": "ssMIDdle", ssMIDdle: 5, "6": "ssDouble", ssDouble: 6};
  $mod.$rtti.$Enum("TShiftStateEnum",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TShiftStateEnum});
  $mod.$rtti.$Set("TShiftState",{comptype: $mod.$rtti["TShiftStateEnum"]});
  $mod.$rtti.$MethodVar("TKeyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.nativeint,1],["Shift",$mod.$rtti["TShiftState"]]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TKeyPressEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.char,1]]), methodkind: 0});
  this.TMouseButton = {"0": "mbLeft", mbLeft: 0, "1": "mbRight", mbRight: 1, "2": "mbMiddle", mbMiddle: 2};
  $mod.$rtti.$Enum("TMouseButton",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TMouseButton});
  $mod.$rtti.$MethodVar("TMouseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",$mod.$rtti["TMouseButton"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseMoveEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseWheelEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",$mod.$rtti["TShiftState"]],["WheelDelta",rtl.nativeint],["MousePos",pas.Types.$rtti["TPoint"]],["Handled",rtl.boolean,1]]), methodkind: 0});
  this.TFocusSearchDirection = {"0": "fsdFirst", fsdFirst: 0, "1": "fsdLast", fsdLast: 1, "2": "fsdNext", fsdNext: 2, "3": "fsdPrev", fsdPrev: 3};
  rtl.createClass($mod,"TControlBorderSpacing",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FAround = 0;
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
    };
    this.Assign = function (Source) {
      var VSpacing = null;
      if ((Source != null) && $mod.TControlBorderSpacing.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VSpacing = Source;
          this.FAround = VSpacing.FAround;
          this.FBottom = VSpacing.FBottom;
          this.FLeft = VSpacing.FLeft;
          this.FRight = VSpacing.FRight;
          this.FTop = VSpacing.FTop;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
  });
  rtl.createClass($mod,"TControl",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAlign = 0;
      this.FAutoSize = false;
      this.FBorderSpacing = null;
      this.FBorderStyle = $mod.TFormBorderStyle.bsNone;
      this.FCaption = "";
      this.FColor = 0;
      this.FControls = null;
      this.FCursor = 0;
      this.FEnabled = false;
      this.FFont = null;
      this.FHandleClass = "";
      this.FHandleElement = null;
      this.FHandleId = "";
      this.FHeight = 0;
      this.FHint = "";
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = false;
      this.FParentShowHint = false;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = false;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = false;
      this.FWidth = 0;
      this.FOnClick = null;
      this.FOnDblClick = null;
      this.FOnMouseDown = null;
      this.FOnMouseEnter = null;
      this.FOnMouseLeave = null;
      this.FOnMouseMove = null;
      this.FOnMouseUp = null;
      this.FOnMouseWheel = null;
      this.FOnResize = null;
      this.FOnScroll = null;
    };
    this.$final = function () {
      this.FBorderSpacing = undefined;
      this.FControls = undefined;
      this.FFont = undefined;
      this.FHandleElement = undefined;
      this.FParent = undefined;
      this.FOnClick = undefined;
      this.FOnDblClick = undefined;
      this.FOnMouseDown = undefined;
      this.FOnMouseEnter = undefined;
      this.FOnMouseLeave = undefined;
      this.FOnMouseMove = undefined;
      this.FOnMouseUp = undefined;
      this.FOnMouseWheel = undefined;
      this.FOnResize = undefined;
      this.FOnScroll = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetClientHeight = function () {
      var Result = 0;
      Result = this.GetClientRect().Bottom;
      return Result;
    };
    this.GetClientRect = function () {
      var Result = new pas.Types.TRect();
      Result = new pas.Types.TRect(pas.Types.Rect(0,0,this.FWidth,this.FHeight));
      return Result;
    };
    this.GetClientWidth = function () {
      var Result = 0;
      Result = this.GetClientRect().Right;
      return Result;
    };
    this.GetText = function () {
      var Result = "";
      Result = this.RealGetText();
      return Result;
    };
    this.SetAlign = function (AValue) {
      if (this.FAlign !== AValue) {
        this.FAlign = AValue;
        this.ReAlign();
      };
    };
    this.SetAutoSize = function (AValue) {
      if (this.FAutoSize !== AValue) {
        this.FAutoSize = AValue;
        if (this.FAutoSize) {
          this.AdjustSize();
        };
      };
    };
    this.SetBorderSpacing = function (AValue) {
      this.FBorderSpacing.Assign(AValue);
    };
    this.SetClientSize = function (AValue) {
      var VClient = new pas.Types.TRect();
      VClient = new pas.Types.TRect(this.GetClientRect());
      this.SetBounds(this.FLeft,this.FTop,(this.FWidth - VClient.Right) + AValue.x,(this.FHeight - VClient.Bottom) + AValue.y);
    };
    this.SetClientHeight = function (AValue) {
      this.SetClientSize(new pas.Types.TPoint(pas.Types.Point(this.GetClientWidth(),AValue)));
    };
    this.SetClientWidth = function (AValue) {
      this.SetClientSize(new pas.Types.TPoint(pas.Types.Point(AValue,this.GetClientHeight())));
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.FParentColor = false;
        this.ColorChanged(this);
      };
    };
    this.SetCursor = function (AValue) {
      if (this.FCursor !== AValue) {
        this.FCursor = AValue;
        this.Changed();
      };
    };
    this.SetEnabled = function (AValue) {
      if (this.FEnabled !== AValue) {
        this.FEnabled = AValue;
        this.Changed();
      };
    };
    this.SetFont = function (AValue) {
      if (!this.FFont.IsEqual(AValue)) {
        this.FFont.Assign(AValue);
      };
    };
    this.SetHandleClass = function (AValue) {
      if (this.FHandleClass !== AValue) {
        this.FHandleClass = AValue;
        this.Changed();
      };
    };
    this.SetHandleId = function (AValue) {
      if (this.FHandleId !== AValue) {
        this.FHandleId = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,this.FWidth,AValue);
    };
    this.SetHint = function (AValue) {
      if (this.FHint !== AValue) {
        this.FHint = AValue;
        this.Changed();
      };
    };
    this.SetLeft = function (AValue) {
      this.SetBounds(AValue,this.FTop,this.FWidth,this.FHeight);
    };
    this.SetParent = function (AValue) {
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      this.CheckNewParent(AValue);
      this.FParent = AValue;
      if (this.FParent != null) {
        this.FParent.RegisterChild(this);
        this.BeginUpdate();
        try {
          if (this.FParentColor) {
            this.FColor = this.FParent.FColor;
          };
          if (this.FParentFont) {
            this.FFont.Assign(this.FParent.FFont);
          };
          if (this.FParentShowHint) {
            this.FShowHint = this.FParent.FShowHint;
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetParentColor = function (AValue) {
      if (this.FParentColor !== AValue) {
        this.FParentColor = AValue;
        if (this.FParentColor && (this.FParent != null)) {
          this.FColor = this.FParent.FColor;
          this.Changed();
        };
      };
    };
    this.SetParentFont = function (AValue) {
      if (this.FParentFont !== AValue) {
        this.FParentFont = AValue;
        if ((this.FParentFont && (this.FParent != null)) && !this.FFont.IsEqual(this.FParent.FFont)) {
          this.FFont.Assign(this.FParent.FFont);
        };
      };
    };
    this.SetParentShowHint = function (AValue) {
      if (this.FParentShowHint !== AValue) {
        this.FParentShowHint = AValue;
        if (this.FParentShowHint && (this.FParent != null)) {
          this.FShowHint = this.FParent.FShowHint;
          this.Changed();
        };
      };
    };
    this.SetShowHint = function (AValue) {
      if (this.FShowHint !== AValue) {
        this.FShowHint = AValue;
        this.FParentShowHint = false;
        this.Changed();
      };
    };
    this.SetTabOrder = function (AValue) {
      if (this.FTabOrder !== AValue) {
        this.FTabOrder = AValue;
        if (this.FParent != null) {
          this.FParent.UpdateTabOrder(this);
        };
      };
    };
    this.SetTabStop = function (AValue) {
      if (this.FTabStop !== AValue) {
        this.FTabStop = AValue;
        this.Changed();
      };
    };
    this.SetText = function (AValue) {
      this.RealSetText(AValue);
    };
    this.SetTop = function (AValue) {
      this.SetBounds(this.FLeft,AValue,this.FWidth,this.FHeight);
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.ReAlign();
      };
    };
    this.SetWidth = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,AValue,this.FHeight);
    };
    this.Click = function () {
      if (this.FOnClick != null) {
        this.FOnClick(this);
      };
    };
    this.DblClick = function () {
      if (this.FOnDblClick != null) {
        this.FOnDblClick(this);
      };
    };
    this.DoResize = function () {
      if (this.FOnScroll != null) {
        this.FOnScroll(this);
      };
    };
    this.DoScroll = function () {
      if (this.FOnScroll != null) {
        this.FOnScroll(this);
      };
    };
    this.MouseDown = function (Button, Shift, X, Y) {
      if (this.FOnMouseDown != null) {
        this.FOnMouseDown(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseEnter = function () {
      if (this.FOnMouseEnter != null) {
        this.FOnMouseEnter(this);
      };
    };
    this.MouseLeave = function () {
      if (this.FOnMouseLeave != null) {
        this.FOnMouseLeave(this);
      };
    };
    this.MouseMove = function (Shift, X, Y) {
      if (this.FOnMouseMove != null) {
        this.FOnMouseMove(this,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseUp = function (Button, Shift, X, Y) {
      if (this.FOnMouseUp != null) {
        this.FOnMouseUp(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseWeel = function (Shift, WheelDelta, MousePos, Handled) {
      if (this.FOnMouseWheel != null) {
        this.FOnMouseWheel(this,rtl.refSet(Shift),WheelDelta,new pas.Types.TPoint(MousePos),Handled);
      };
    };
    this.HandleClick = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.Click();
      Result = true;
      return Result;
    };
    this.HandleDblClick = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DblClick();
      Result = true;
      return Result;
    };
    this.HandleMouseDown = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = new pas.Types.TRect();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets = new pas.Types.TRect($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      AEvent.stopPropagation();
      this.MouseDown(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseEnter = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.MouseEnter();
      Result = true;
      return Result;
    };
    this.HandleMouseLeave = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.MouseLeave();
      Result = true;
      return Result;
    };
    this.HandleMouseMove = function (AEvent) {
      var Result = false;
      var VOffSets = new pas.Types.TRect();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VOffSets = new pas.Types.TRect($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Left);
      AEvent.stopPropagation();
      this.MouseMove(rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseUp = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = new pas.Types.TRect();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets = new pas.Types.TRect($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      AEvent.stopPropagation();
      this.MouseUp(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseWheel = function (AEvent) {
      var Result = false;
      var VDelta = 0;
      var VHandled = false;
      var VMousePos = new pas.Types.TPoint();
      var VShift = {};
      var VOffSets = new pas.Types.TRect();
      VDelta = pas.System.Trunc(-AEvent.deltaY);
      VHandled = false;
      VOffSets = new pas.Types.TRect($mod.OffSets(this.FHandleElement));
      VMousePos = new pas.Types.TPoint(pas.Types.Point(VOffSets.Left,VOffSets.Top));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      AEvent.stopPropagation();
      this.MouseWeel(rtl.refSet(VShift),VDelta,new pas.Types.TPoint(VMousePos),{get: function () {
          return VHandled;
        }, set: function (v) {
          VHandled = v;
        }});
      Result = true;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoResize();
      Result = true;
      return Result;
    };
    this.HandleScroll = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoScroll();
      Result = true;
      return Result;
    };
    this.Changed = function () {
      if (!this.IsUpdating()) {
        var $with1 = this.FHandleElement;
        if (this.FHandleId !== "") {
          $with1.setAttribute("id",this.FHandleId);
        } else {
          $with1.removeAttribute("id");
        };
        if (this.FHandleClass !== "") {
          $with1.setAttribute("class",this.FHandleClass);
        } else {
          $with1.removeAttribute("class");
        };
        if ((this.FHandleClass === "") && (this.FHandleId === "")) {
          $with1.style.setProperty("color",pas.Graphics.JSColor(this.FFont.FColor));
          $with1.style.setProperty("font-family",this.FFont.FName);
          $with1.style.setProperty("font-size",pas.SysUtils.IntToStr(this.FFont.FSize) + "px");
          $with1.style.setProperty("font-style","normal");
          $with1.style.setProperty("font-weight",$mod.IfThen$3(0 in this.FFont.FStyle,"bold","normal"));
          $with1.style.setProperty("text-decoration",$mod.IfThen$3(2 in this.FFont.FStyle,"underline","normal"));
          if (this.FColor in rtl.createSet(536870912,536870911)) {
            $with1.style.removeProperty("background-color");
          } else {
            $with1.style.setProperty("background-color",pas.Graphics.JSColor(this.FColor));
          };
        };
        $with1.style.setProperty("left",pas.SysUtils.IntToStr(this.FLeft) + "px");
        $with1.style.setProperty("top",pas.SysUtils.IntToStr(this.FTop) + "px");
        $with1.style.setProperty("width",pas.SysUtils.IntToStr(this.FWidth) + "px");
        $with1.style.setProperty("height",pas.SysUtils.IntToStr(this.FHeight) + "px");
        $with1.style.setProperty("cursor",$mod.JSCursor(this.FCursor));
        if (this.FEnabled) {
          $with1.removeAttribute("disabled");
          $with1.style.removeProperty("opacity");
        } else {
          $with1.setAttribute("disabled","true");
          $with1.style.setProperty("opacity","0.5");
        };
        if (this.FVisible) {
          $with1.style.setProperty("visibility","visible");
          $with1.style.setProperty("display","block");
        } else {
          $with1.style.setProperty("visibility","hidden");
          $with1.style.setProperty("display","none");
        };
        if ((this.FHint !== "") && this.FShowHint) {
          $with1.setAttribute("title",this.FHint);
        } else {
          $with1.removeAttribute("title");
        };
        if (this.FBorderStyle === 0) {
          $with1.style.setProperty("border-style","none");
        } else {
          $with1.style.removeProperty("border-style");
        };
        $with1.setAttribute("tabindex",$mod.IfThen$3(this.FTabStop,"1","-1"));
        $with1.style.setProperty("position","absolute");
        $with1.style.setProperty("overflow","hidden");
        $with1.style.setProperty("-webkit-box-sizing","border-box");
        $with1.style.setProperty("-moz-box-sizing","border-box");
        $with1.style.setProperty("box-sizing","border-box");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      throw new Error(pas.SysUtils.Format("%s.CreateHandleElement=nil",[this.$classname]));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      var $with1 = this.FHandleElement;
      $with1.addEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with1.addEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with1.addEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with1.addEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with1.addEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with1.addEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with1.addEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with1.addEventListener("scroll",rtl.createCallback(this,"HandleScroll"));
      $with1.addEventListener("resize",rtl.createCallback(this,"HandleResize"));
      $with1.addEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
    };
    this.UnRegisterHandleEvents = function () {
      var $with1 = this.FHandleElement;
      $with1.removeEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with1.removeEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with1.removeEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with1.removeEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with1.removeEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with1.removeEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with1.removeEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with1.removeEventListener("scroll",rtl.createCallback(this,"HandleScroll"));
      $with1.removeEventListener("resize",rtl.createCallback(this,"HandleResize"));
      $with1.removeEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
    };
    this.CheckNewParent = function (AParent) {
      if ((AParent != null) && !AParent.CheckChildClassAllowed(this.$class.ClassType())) {
        throw new Error(pas.SysUtils.Format("Control of class '%s' can't have control of class '%s' as a child",[AParent.$class.ClassType(),this.$classname]));
      };
      if (pas.Forms.TCustomForm.isPrototypeOf(this) && pas.Forms.TCustomForm.isPrototypeOf(AParent)) {
        throw new Error('A "Form" can\'t have another "Form" as parent');
      };
      if (this === AParent) {
        throw new Error('A "Control" can\'t have itself as a Parent');
      };
    };
    this.RegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex < 0) {
          this.FControls.push(AControl);
          if (!this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.appendChild(AControl.FHandleElement);
          };
          this.ReAlign();
          AControl.SetTabOrder(this.FControls.length);
        };
      };
    };
    this.UnRegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex >= 0) {
          this.FControls.splice(VIndex,1);
          if (this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.removeChild(AControl.FHandleElement);
          };
          this.ReAlign();
          this.UpdateTabOrder(null);
        };
      };
    };
    this.AlignControls = function () {
      var VControl = null;
      var VSpacing = null;
      var VIndex = 0;
      var VLeft = 0;
      var VTop = 0;
      var VRight = 0;
      var VBotton = 0;
      var VWidth = 0;
      this.BeginUpdate();
      try {
        VLeft = 0;
        VTop = 0;
        VRight = this.FWidth;
        VBotton = this.FHeight;
        VWidth = this.FWidth;
        for (var $l1 = 0, $end2 = this.FControls.length - 1; $l1 <= $end2; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl.FAlign === 1)) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft((VLeft + VSpacing.FLeft) + VSpacing.FAround);
              VControl.SetTop((VTop + VSpacing.FTop) + VSpacing.FAround);
              VControl.SetWidth(((VWidth - VSpacing.FLeft) - VSpacing.FRight) - (VSpacing.FAround * 2));
              VControl.SetHeight(VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
            VTop = (((VTop + VControl.FHeight) + VSpacing.FTop) + VSpacing.FBottom) + (VSpacing.FAround * 2);
          };
        };
        if (VTop < 0) {
          VTop = 0;
        };
        for (var $l3 = 0, $end4 = this.FControls.length - 1; $l3 <= $end4; $l3++) {
          VIndex = $l3;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl.FAlign === 2)) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft((VLeft + VSpacing.FLeft) + VSpacing.FAround);
              VControl.SetTop(((VBotton - VControl.FHeight) - VSpacing.FBottom) - VSpacing.FAround);
              VControl.SetWidth(((VWidth - VSpacing.FLeft) - VSpacing.FRight) - (VSpacing.FAround * 2));
              VControl.SetHeight(VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
            VBotton = (((VBotton - VControl.FHeight) - VSpacing.FTop) - VSpacing.FBottom) - (VSpacing.FAround * 2);
          };
        };
        if (VBotton < 0) {
          VBotton = 0;
        };
        for (var $l5 = 0, $end6 = this.FControls.length - 1; $l5 <= $end6; $l5++) {
          VIndex = $l5;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl.FAlign === 3)) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft((VLeft + VSpacing.FLeft) + VSpacing.FAround);
              VControl.SetTop((VTop + VSpacing.FTop) + VSpacing.FAround);
              VControl.SetWidth(VControl.FWidth);
              VControl.SetHeight((((VBotton - VTop) - VSpacing.FTop) - VSpacing.FBottom) - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VLeft = (((VLeft + VControl.FWidth) + VSpacing.FLeft) + VSpacing.FRight) + (VSpacing.FAround * 2);
          };
        };
        if (VLeft < 0) {
          VLeft = 0;
        };
        for (var $l7 = 0, $end8 = this.FControls.length - 1; $l7 <= $end8; $l7++) {
          VIndex = $l7;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl.FAlign === 4)) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(((VRight - VControl.FWidth) - VSpacing.FRight) - VSpacing.FAround);
              VControl.SetTop((VTop + VSpacing.FTop) + VSpacing.FAround);
              VControl.SetWidth(VControl.FWidth);
              VControl.SetHeight((((VBotton - VTop) - VSpacing.FTop) - VSpacing.FBottom) - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VRight = (((VRight - VControl.FWidth) - VSpacing.FLeft) - VSpacing.FRight) - (VSpacing.FAround * 2);
          };
        };
        if (VRight < 0) {
          VRight = 0;
        };
        for (var $l9 = 0, $end10 = this.FControls.length - 1; $l9 <= $end10; $l9++) {
          VIndex = $l9;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl.FAlign === 5)) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft((VLeft + VSpacing.FLeft) + VSpacing.FAround);
              VControl.SetTop((VTop + VSpacing.FTop) + VSpacing.FAround);
              VControl.SetWidth((((VRight - VLeft) - VSpacing.FLeft) - VSpacing.FRight) - (VSpacing.FAround * 2));
              VControl.SetHeight((((VBotton - VTop) - VSpacing.FTop) - VSpacing.FBottom) - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        this.EndUpdate();
      };
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FCaption;
      return Result;
    };
    this.RealSetText = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.Changed();
      };
    };
    this.BorderSpacingChanged = function (Sender) {
      if (this.FParent != null) {
        this.FParent.AlignControls();
      };
    };
    this.ColorChanged = function (Sender) {
      this.Changed();
    };
    this.FontChanged = function (Sender) {
      this.Changed();
    };
    this.TabOrderArray = function () {
      var Result = null;
      Result = this.FControls.slice(0).sort(rtl.createCallback(this,"CompareTabOrder"));
      return Result;
    };
    this.CompareTabOrder = function (A, B) {
      var Result = 0;
      if (((pas.System.Assigned(A) && pas.System.Assigned(B)) && rtl.isExt(A,$mod.TControl,1)) && rtl.isExt(B,$mod.TControl,1)) {
        Result = rtl.getObject(A).FTabOrder - rtl.getObject(B).FTabOrder;
      } else {
        Result = 0;
      };
      return Result;
    };
    this.UpdateTabOrder = function (AValue) {
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      if (AValue != null) {
        for (var $l1 = 0, $end2 = this.FControls.length - 1; $l1 <= $end2; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if (((VControl != null) && (VControl !== AValue)) && (VControl.FTabOrder >= AValue.FTabOrder)) {
            VControl.FTabOrder += 1;
          };
        };
      };
      VArray = this.TabOrderArray();
      try {
        for (var $l3 = 0, $end4 = VArray.length - 1; $l3 <= $end4; $l3++) {
          VIndex = $l3;
          VControl = rtl.getObject(VArray[VIndex]);
          if (VControl != null) {
            VControl.BeginUpdate();
            try {
              VControl.FTabOrder = VIndex;
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FHandleElement = this.CreateHandleElement();
      this.FHandleClass = "";
      this.FHandleId = "";
      this.RegisterHandleEvents();
      this.FControls = new Array();
      this.FBorderSpacing = $mod.TControlBorderSpacing.$create("Create$1");
      this.FBorderSpacing.FOnChange = rtl.createCallback(this,"BorderSpacingChanged");
      this.FBorderStyle = 1;
      this.FFont = pas.Graphics.TFont.$create("Create$1");
      this.FFont.FOnChange = rtl.createCallback(this,"FontChanged");
      this.FAlign = 0;
      this.FAutoSize = false;
      this.FCaption = "";
      this.FColor = 536870912;
      this.FCursor = 0;
      this.FEnabled = true;
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = true;
      this.FParentShowHint = true;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = true;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = true;
    };
    this.Destroy = function () {
      this.DestroyComponents();
      this.UnRegisterHandleEvents();
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      this.FControls.length = 0;
      this.FBorderSpacing.$destroy("Destroy");
      this.FBorderSpacing = null;
      this.FFont.$destroy("Destroy");
      this.FFont = null;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.AdjustSize = function () {
    };
    this.IsUpdating = function () {
      var Result = false;
      Result = this.FUpdateCount > 0;
      return Result;
    };
    this.Invalidate = function () {
    };
    this.ReAlign = function () {
      this.AlignControls();
      if (this.FParent != null) {
        this.FParent.ReAlign();
      };
      this.Invalidate();
    };
    this.BringToFront = function () {
      var VParentElement = null;
      VParentElement = this.FHandleElement.parentElement;
      if (VParentElement != null) {
        VParentElement.removeChild(this.FHandleElement);
        VParentElement.appendChild(this.FHandleElement);
      };
    };
    this.SetBounds = function (ALeft, ATop, AWidth, AHeight) {
      if ((((this.FLeft !== ALeft) || (this.FTop !== ATop)) || (this.FWidth !== AWidth)) || (this.FHeight !== AHeight)) {
        this.FLeft = ALeft;
        this.FTop = ATop;
        if (AWidth > 0) {
          this.FWidth = AWidth;
        } else {
          this.FWidth = 0;
        };
        if (AHeight > 0) {
          this.FHeight = AHeight;
        } else {
          this.FHeight = 0;
        };
        this.Changed();
        this.ReAlign();
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Cursor",2,$mod.$rtti["TCursor"],"FCursor","SetCursor");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("Height",2,rtl.nativeint,"FHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
  });
  rtl.createClass($mod,"TWinControl",$mod.TControl,function () {
    this.$init = function () {
      $mod.TControl.$init.call(this);
      this.FOnEnter = null;
      this.FOnExit = null;
      this.FOnKeyDown = null;
      this.FOnKeyPress = null;
      this.FOnKeyUp = null;
    };
    this.$final = function () {
      this.FOnEnter = undefined;
      this.FOnExit = undefined;
      this.FOnKeyDown = undefined;
      this.FOnKeyPress = undefined;
      this.FOnKeyUp = undefined;
      $mod.TControl.$final.call(this);
    };
    this.DoEnter = function () {
      if (this.FOnEnter != null) {
        this.FOnEnter(this);
      };
    };
    this.DoExit = function () {
      if (this.FOnExit != null) {
        this.FOnExit(this);
      };
    };
    this.KeyDown = function (Key, Shift) {
      if (this.FOnKeyDown != null) {
        this.FOnKeyDown(this,Key,rtl.refSet(Shift));
      };
    };
    this.KeyPress = function (Key) {
      if (this.FOnKeyPress != null) {
        this.FOnKeyPress(this,Key);
      };
    };
    this.KeyUp = function (Key, Shift) {
      if (this.FOnKeyUp != null) {
        this.FOnKeyUp(this,Key,rtl.refSet(Shift));
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VParent.SetActiveControl(this);
          break;
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      this.DoEnter();
      Result = true;
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoExit();
      Result = true;
      return Result;
    };
    this.HandleKeyDown = function (AEvent) {
      var Result = false;
      var VControl = null;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyDown(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyDown({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      } else {
        var $tmp1 = VKey;
        if ($tmp1 === 9) {
          if (this.FParent != null) {
            if (0 in VShift) {
              VControl = this.FParent.FindFocusControl(this,3);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,1);
              };
            } else {
              VControl = this.FParent.FindFocusControl(this,2);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,0);
              };
            };
            if ((VControl != null) && VControl.CanSetFocus()) {
              VControl.SetFocus();
            };
            AEvent.preventDefault();
          };
        };
      };
      Result = true;
      return Result;
    };
    this.HandleKeyUp = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyUp(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyUp({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      };
      Result = true;
      return Result;
    };
    this.HandleKeyPress = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = "";
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyPress(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      VKey = $mod.ExtractKeyChar(AEvent);
      if (VKey === "\x00") {
        AEvent.preventDefault();
      } else {
        this.KeyPress({get: function () {
            return VKey;
          }, set: function (v) {
            VKey = v;
          }});
        if (VKey === "\x00") {
          AEvent.preventDefault();
        };
      };
      Result = true;
      return Result;
    };
    this.RegisterHandleEvents = function () {
      $mod.TControl.RegisterHandleEvents.call(this);
      var $with1 = this.FHandleElement;
      $with1.addEventListener("focus",rtl.createCallback(this,"HandleEnter"));
      $with1.addEventListener("blur",rtl.createCallback(this,"HandleExit"));
      $with1.addEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with1.addEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with1.addEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.UnRegisterHandleEvents = function () {
      $mod.TControl.UnRegisterHandleEvents.call(this);
      var $with1 = this.FHandleElement;
      $with1.removeEventListener("focus",rtl.createCallback(this,"HandleEnter"));
      $with1.removeEventListener("blur",rtl.createCallback(this,"HandleExit"));
      $with1.removeEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with1.removeEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with1.removeEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = (AChildClass != null) && AChildClass.InheritsFrom($mod.TControl);
      return Result;
    };
    this.FindFocusControl = function (AStartControl, ADirection) {
      var Result = null;
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      var VTabOrder = 0;
      Result = null;
      VArray = this.TabOrderArray();
      if (VArray.length === 0) {
        return Result;
      };
      try {
        VTabOrder = VArray.indexOf(AStartControl);
        if (VTabOrder < 0) {
          if (ADirection in rtl.createSet(0)) {
            VTabOrder = VArray.length - 1;
          } else {
            VTabOrder = 0;
          };
        };
        var $tmp1 = ADirection;
        if ($tmp1 === 0) {
          VControl = rtl.getObject(VArray[0]);
          if (((((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl)) && VControl.FEnabled) && VControl.FVisible) && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp1 === 1) {
          VControl = rtl.getObject(VArray[VArray.length - 1]);
          if (((((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl)) && VControl.FEnabled) && VControl.FVisible) && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp1 === 2) {
          if (VTabOrder < (VArray.length - 1)) {
            for (var $l2 = VTabOrder + 1, $end3 = VArray.length - 1; $l2 <= $end3; $l2++) {
              VIndex = $l2;
              VControl = rtl.getObject(VArray[VIndex]);
              if (((((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl)) && VControl.FEnabled) && VControl.FVisible) && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        } else if ($tmp1 === 3) {
          if (VTabOrder > 0) {
            for (var $l4 = VTabOrder - 1; $l4 >= 0; $l4--) {
              VIndex = $l4;
              VControl = rtl.getObject(VArray[VIndex]);
              if (((((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl)) && VControl.FEnabled) && VControl.FVisible) && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
      return Result;
    };
    this.CanSetFocus = function () {
      var Result = false;
      var VControl = null;
      VControl = this;
      while (true) {
        if (!VControl.FVisible && VControl.FEnabled) {
          Result = false;
          return Result;
        };
        if (VControl.FParent != null) {
          VControl = VControl.FParent;
        } else {
          break;
        };
      };
      Result = (VControl != null) && pas.Forms.TCustomForm.isPrototypeOf(VControl);
      return Result;
    };
    this.SetFocus = function () {
      this.FHandleElement.focus();
    };
  });
  rtl.createClass($mod,"TCustomControl",$mod.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FCanvas = null;
      this.FOnPaint = null;
    };
    this.$final = function () {
      this.FCanvas = undefined;
      this.FOnPaint = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.ColorChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FBrush.SetColor(this.FColor);
      };
      $mod.TControl.ColorChanged.call(this,Sender);
    };
    this.FontChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FFont.Assign(this.FFont);
      };
      $mod.TControl.FontChanged.call(this,Sender);
    };
    this.Paint = function () {
      if (this.FOnPaint != null) {
        this.FOnPaint(this);
      };
    };
    this.Destroy = function () {
      if (this.FCanvas != null) {
        this.FCanvas.$destroy("Destroy");
        this.FCanvas = null;
      };
      $mod.TControl.Destroy.call(this);
    };
    this.Invalidate = function () {
      $mod.TControl.Invalidate.call(this);
      this.Paint();
    };
  });
  this.IfThen$3 = function (AExpression, AConsequence, AAlternative) {
    var Result = "";
    if (AExpression) {
      Result = AConsequence;
    } else {
      Result = AAlternative;
    };
    return Result;
  };
  this.OffSets = function (AElement) {
    var Result = new pas.Types.TRect();
    Result = new pas.Types.TRect(pas.Types.Rect(0,0,0,0));
    if (AElement != null) {
      var $with1 = AElement.getBoundingClientRect();
      Result.Left = pas.System.Trunc($with1.left + window.scrollX);
      Result.Top = pas.System.Trunc($with1.top + window.screenY);
    };
    return Result;
  };
  this.ExtractKeyCode = function (AEvent) {
    var Result = 0;
    var VLocation = 0;
    var VKey = "";
    VLocation = AEvent.location;
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = -1;
    var $tmp1 = VKey;
    if ($tmp1 === "backspace") {
      Result = 8}
     else if ($tmp1 === "tab") {
      Result = 9}
     else if ($tmp1 === "enter") {
      Result = 13}
     else if ($tmp1 === "shift") {
      Result = 16}
     else if ($tmp1 === "control") {
      Result = 17}
     else if ($tmp1 === "alt") {
      Result = 18}
     else if ($tmp1 === "altgraph") {
      Result = 18}
     else if ($tmp1 === "pause") {
      Result = 19}
     else if ($tmp1 === "capslock") {
      Result = 20}
     else if ($tmp1 === "escape") {
      Result = 27}
     else if ($tmp1 === "pageup") {
      Result = 33}
     else if ($tmp1 === "pagedown") {
      Result = 34}
     else if ($tmp1 === "end") {
      Result = 35}
     else if ($tmp1 === "home") {
      Result = 36}
     else if ($tmp1 === "arrowleft") {
      Result = 37}
     else if ($tmp1 === "arrowup") {
      Result = 38}
     else if ($tmp1 === "arrowright") {
      Result = 39}
     else if ($tmp1 === "arrowdown") {
      Result = 40}
     else if ($tmp1 === "insert") {
      Result = 45}
     else if ($tmp1 === "delete") {
      Result = 46}
     else if ($tmp1 === "f1") {
      Result = 112}
     else if ($tmp1 === "f2") {
      Result = 113}
     else if ($tmp1 === "f3") {
      Result = 114}
     else if ($tmp1 === "f4") {
      Result = 115}
     else if ($tmp1 === "f5") {
      Result = 116}
     else if ($tmp1 === "f6") {
      Result = 117}
     else if ($tmp1 === "f7") {
      Result = 118}
     else if ($tmp1 === "f8") {
      Result = 119}
     else if ($tmp1 === "f9") {
      Result = 120}
     else if ($tmp1 === "f10") {
      Result = 121}
     else if ($tmp1 === "f11") {
      Result = 122}
     else if ($tmp1 === "f12") {
      Result = 123}
     else if ($tmp1 === "f13") {
      Result = 124}
     else if ($tmp1 === "f14") {
      Result = 125}
     else if ($tmp1 === "f15") {
      Result = 126}
     else if ($tmp1 === "f16") {
      Result = 127}
     else if ($tmp1 === "f17") {
      Result = 128}
     else if ($tmp1 === "f18") {
      Result = 129}
     else if ($tmp1 === "f19") {
      Result = 130}
     else if ($tmp1 === "f20") {
      Result = 131}
     else if ($tmp1 === "numlock") {
      Result = 144}
     else if ($tmp1 === "scrolllock") Result = 145;
    if (VLocation === 3) {
      var $tmp2 = VKey;
      if ($tmp2 === "0") {
        Result = 96}
       else if ($tmp2 === "1") {
        Result = 97}
       else if ($tmp2 === "2") {
        Result = 98}
       else if ($tmp2 === "3") {
        Result = 99}
       else if ($tmp2 === "4") {
        Result = 100}
       else if ($tmp2 === "5") {
        Result = 101}
       else if ($tmp2 === "6") {
        Result = 102}
       else if ($tmp2 === "7") {
        Result = 103}
       else if ($tmp2 === "8") {
        Result = 104}
       else if ($tmp2 === "9") {
        Result = 105}
       else if ($tmp2 === "*") {
        Result = 106}
       else if ($tmp2 === "+") {
        Result = 107}
       else if ($tmp2 === "-") {
        Result = 109}
       else if ($tmp2 === ",") {
        Result = 110}
       else if ($tmp2 === "\/") {
        Result = 111}
       else if ($tmp2 === ".") Result = 194;
    } else {
      var $tmp3 = VKey;
      if ($tmp3 === "0") {
        Result = 48}
       else if ($tmp3 === "1") {
        Result = 49}
       else if ($tmp3 === "2") {
        Result = 50}
       else if ($tmp3 === "3") {
        Result = 51}
       else if ($tmp3 === "4") {
        Result = 52}
       else if ($tmp3 === "5") {
        Result = 53}
       else if ($tmp3 === "6") {
        Result = 54}
       else if ($tmp3 === "7") {
        Result = 55}
       else if ($tmp3 === "8") {
        Result = 56}
       else if ($tmp3 === "9") {
        Result = 57}
       else if ($tmp3 === "ç") {
        Result = 63}
       else if ($tmp3 === "a") {
        Result = 65}
       else if ($tmp3 === "b") {
        Result = 66}
       else if ($tmp3 === "c") {
        Result = 67}
       else if ($tmp3 === "d") {
        Result = 68}
       else if ($tmp3 === "e") {
        Result = 69}
       else if ($tmp3 === "f") {
        Result = 70}
       else if ($tmp3 === "g") {
        Result = 71}
       else if ($tmp3 === "h") {
        Result = 72}
       else if ($tmp3 === "i") {
        Result = 73}
       else if ($tmp3 === "j") {
        Result = 74}
       else if ($tmp3 === "k") {
        Result = 75}
       else if ($tmp3 === "l") {
        Result = 76}
       else if ($tmp3 === "m") {
        Result = 77}
       else if ($tmp3 === "n") {
        Result = 78}
       else if ($tmp3 === "o") {
        Result = 79}
       else if ($tmp3 === "p") {
        Result = 80}
       else if ($tmp3 === "q") {
        Result = 81}
       else if ($tmp3 === "r") {
        Result = 82}
       else if ($tmp3 === "s") {
        Result = 83}
       else if ($tmp3 === "t") {
        Result = 84}
       else if ($tmp3 === "u") {
        Result = 85}
       else if ($tmp3 === "v") {
        Result = 86}
       else if ($tmp3 === "w") {
        Result = 87}
       else if ($tmp3 === "x") {
        Result = 88}
       else if ($tmp3 === "y") {
        Result = 89}
       else if ($tmp3 === "z") {
        Result = 90}
       else if ($tmp3 === "=") {
        Result = 187}
       else if ($tmp3 === ",") {
        Result = 188}
       else if ($tmp3 === "-") {
        Result = 189}
       else if ($tmp3 === ".") {
        Result = 190}
       else if ($tmp3 === "'") {
        Result = 192}
       else if ($tmp3 === "\/") {
        Result = 193}
       else if ($tmp3 === "]") {
        Result = 220}
       else if ($tmp3 === "[") Result = 221;
    };
    return Result;
  };
  this.ExtractKeyChar = function (AEvent) {
    var Result = "";
    var VKey = "";
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = "\x00";
    if (VKey.length === 1) {
      Result = VKey.charAt(0);
    } else {
      var $tmp1 = VKey;
      if ($tmp1 === "backspace") {
        Result = "\b"}
       else if ($tmp1 === "tab") {
        Result = "\t"}
       else if ($tmp1 === "enter") {
        Result = "\r"}
       else if ($tmp1 === "escape") Result = "\x1B";
    };
    return Result;
  };
  this.ExtractShiftState = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractShiftState$1 = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractMouseButton = function (AEvent) {
    var Result = 0;
    var $tmp1 = AEvent.button;
    if ($tmp1 === 1) {
      Result = 2}
     else if ($tmp1 === 2) {
      Result = 1}
     else {
      Result = 2;
    };
    return Result;
  };
  this.JSCursor = function (ACursor) {
    var Result = "";
    var $tmp1 = ACursor;
    if ($tmp1 === -1) {
      Result = "none"}
     else if ($tmp1 === -3) {
      Result = "crosshair"}
     else if ($tmp1 === -4) {
      Result = "text"}
     else if ($tmp1 === -22) {
      Result = "move"}
     else if ($tmp1 === -6) {
      Result = "nesw-resize"}
     else if ($tmp1 === -7) {
      Result = "ns-resize"}
     else if ($tmp1 === -8) {
      Result = "nwse-resize"}
     else if ($tmp1 === -9) {
      Result = "ew-resize"}
     else if ($tmp1 === -23) {
      Result = "nwse-resize"}
     else if ($tmp1 === -24) {
      Result = "ns-resize"}
     else if ($tmp1 === -25) {
      Result = "nesw-resize"}
     else if ($tmp1 === -26) {
      Result = "col-resize"}
     else if ($tmp1 === -27) {
      Result = "col-resize"}
     else if ($tmp1 === -28) {
      Result = "nesw-resize"}
     else if ($tmp1 === -29) {
      Result = "ns-resize"}
     else if ($tmp1 === -30) {
      Result = "nwse-resize"}
     else if ($tmp1 === -11) {
      Result = "wait"}
     else if ($tmp1 === -13) {
      Result = "no-drop"}
     else if ($tmp1 === -14) {
      Result = "col-resize"}
     else if ($tmp1 === -15) {
      Result = "row-resize"}
     else if ($tmp1 === -17) {
      Result = "progress"}
     else if ($tmp1 === -18) {
      Result = "not-allowed"}
     else if ($tmp1 === -19) {
      Result = "wait"}
     else if ($tmp1 === -20) {
      Result = "help"}
     else if ($tmp1 === -21) {
      Result = "pointer"}
     else {
      Result = "";
    };
    return Result;
  };
},["Forms"]);
rtl.module("Forms",["System","Classes","SysUtils","Types","JS","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TFormType = {"0": "ftModalForm", ftModalForm: 0, "1": "ftWindow", ftWindow: 1};
  this.TCloseAction = {"0": "caNone", caNone: 0, "1": "caHide", caHide: 1, "2": "caFree", caFree: 2};
  $mod.$rtti.$Enum("TCloseAction",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TCloseAction});
  $mod.$rtti.$MethodVar("TCloseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CloseAction",$mod.$rtti["TCloseAction"],1]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TCloseQueryEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CanClose",rtl.boolean,1]]), methodkind: 0});
  $mod.$rtti.$Int("TModalResult",{minvalue: -2147483648, maxvalue: 2147483647, ordtype: 4});
  rtl.createClass($mod,"TCustomForm",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 0;
      this.FChildForm = null;
      this.FFormType = 0;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.FOnActivate = null;
      this.FOnClose = null;
      this.FOnCloseQuery = null;
      this.FOnCreate = null;
      this.FOnDeactivate = null;
      this.FOnDestroy = null;
      this.FOnHide = null;
      this.FOnResize$1 = null;
      this.FOnScroll$1 = null;
      this.FOnShow = null;
    };
    this.$final = function () {
      this.FActiveControl = undefined;
      this.FChildForm = undefined;
      this.FModalResultProc = undefined;
      this.FOverlay = undefined;
      this.FOnActivate = undefined;
      this.FOnClose = undefined;
      this.FOnCloseQuery = undefined;
      this.FOnCreate = undefined;
      this.FOnDeactivate = undefined;
      this.FOnDestroy = undefined;
      this.FOnHide = undefined;
      this.FOnResize$1 = undefined;
      this.FOnScroll$1 = undefined;
      this.FOnShow = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.SetActiveControl = function (AValue) {
      if (this.FActiveControl !== AValue) {
        this.FActiveControl = AValue;
      };
    };
    this.SetAlphaBlend = function (AValue) {
      if (this.FAlphaBlend !== AValue) {
        this.FAlphaBlend = AValue;
        this.Changed();
      };
    };
    this.SetAlphaBlendValue = function (AValue) {
      if (this.FAlphaBlendValue !== AValue) {
        this.FAlphaBlendValue = AValue;
        this.Changed();
      };
    };
    this.SetModalResult = function (AValue) {
      if (this.FModalResult !== AValue) {
        this.FModalResult = AValue;
        if ((this.FModalResult !== 0) && (this.FModalResultProc != null)) {
          this.Close();
        };
      };
    };
    this.Activate = function () {
      if (this.FOnActivate != null) {
        this.FOnActivate(this);
      };
    };
    this.Deactivate = function () {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoClose = function (CloseAction) {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoCreate = function () {
      if (this.FOnCreate != null) {
        this.FOnCreate(this);
      };
    };
    this.DoDestroy = function () {
      if (this.FOnDestroy != null) {
        this.FOnDestroy(this);
      };
    };
    this.DoHide = function () {
      if (this.FOnHide != null) {
        this.FOnHide(this);
      };
    };
    this.DoResize = function () {
      pas.Controls.TControl.DoResize.call(this);
      if (this.FOnResize$1 != null) {
        this.FOnResize$1(this);
      };
    };
    this.DoShow = function () {
      if (this.FOnShow != null) {
        this.FOnShow(this);
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VControl = null;
      Result = pas.Controls.TWinControl.HandleEnter.call(this,AEvent);
      if ((this.FChildForm != null) && (this.FChildForm.FFormType === 0)) {
        this.FChildForm.Show();
      } else {
        if (this.FActiveControl != null) {
          VControl = this.FActiveControl;
        } else {
          VControl = this.FindFocusControl(null,0);
        };
        this.FocusControl(VControl);
        this.Activate();
      };
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      Result = pas.Controls.TWinControl.HandleExit.call(this,AEvent);
      this.Deactivate();
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating()) {
        var $with1 = this.FHandleElement;
        $with1.style.setProperty("outline","none");
        if (this.FAlphaBlend) {
          $with1.style.setProperty("opacity",pas.SysUtils.FloatToStr(Math.floor(this.FAlphaBlendValue / 255)));
        } else {
          $with1.style.removeProperty("opacity");
        };
        $with1.style.setProperty("overflow","auto");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = new pas.Types.TSize();
      Result.cx = 320;
      Result.cy = 240;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 255;
      this.FChildForm = null;
      this.FFormType = 1;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.BeginUpdate();
      try {
        this.SetColor(16777215);
        this.SetParentFont(false);
        this.SetParentShowHint(false);
        this.SetVisible(false);
        var $with1 = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with1.cx,$with1.cy);
      } finally {
        this.EndUpdate();
      };
    };
    this.Destroy = function () {
      this.FActiveControl = null;
      this.FChildForm = null;
      pas.Controls.TCustomControl.Destroy.call(this);
    };
    this.AfterConstruction = function () {
      pas.System.TObject.AfterConstruction.call(this);
      $mod.Application().UpdateMainForm(this);
      $mod.Application().RegisterForm(this);
      this.Loaded();
      this.DoCreate();
    };
    this.BeforeDestruction = function () {
      pas.Classes.TComponent.BeforeDestruction.call(this);
      $mod.Application().UnRegisterForm(this);
      this.DoDestroy();
    };
    this.Close = function () {
      var VAction = 0;
      var VIndex = 0;
      var VLastForm = null;
      var VOwnerForm = null;
      if (this.CloseQuery()) {
        VAction = 1;
        this.DoClose({get: function () {
            return VAction;
          }, set: function (v) {
            VAction = v;
          }});
        if (VAction !== 0) {
          if ($mod.Application().FMainForm === this) {
            $mod.Application().Terminate();
          } else {
            this.Hide();
            if (this.FFormType === 0) {
              if ((this.FOwner != null) && $mod.TCustomForm.isPrototypeOf(this.FOwner)) {
                VOwnerForm = this.FOwner;
                VOwnerForm.FChildForm = null;
                if (VOwnerForm.FOverlay != null) {
                  VOwnerForm.FOverlay.$destroy("Destroy");
                  VOwnerForm.FOverlay = null;
                };
                VOwnerForm.Show();
              };
              if (this.FModalResultProc != null) {
                this.FModalResultProc(this,this.FModalResult);
              };
            } else {
              for (var $l1 = $mod.Application().GetFormCount() - 1; $l1 >= 0; $l1--) {
                VIndex = $l1;
                VLastForm = $mod.Application().GetForm(VIndex);
                if (((VLastForm != null) && VLastForm.FVisible) && (VLastForm !== this)) {
                  VLastForm.Show();
                  return;
                };
              };
              if ($mod.Application().FMainForm != null) {
                $mod.Application().FMainForm.Show();
              };
            };
          };
        };
      };
    };
    this.CloseQuery = function () {
      var Result = false;
      Result = true;
      if (this.FOnCloseQuery != null) {
        this.FOnCloseQuery(this,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }});
      };
      return Result;
    };
    this.FocusControl = function (AControl) {
      if ((AControl != null) && AControl.CanSetFocus()) {
        AControl.SetFocus();
      };
    };
    this.Hide = function () {
      this.SetVisible(false);
      this.DoHide();
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.call(this);
    };
    this.Resize = function () {
      var VHeight = 0;
      var VLeft = 0;
      var VTop = 0;
      var VWidth = 0;
      var VWindowHeight = 0;
      var VWindowWidth = 0;
      VWindowWidth = window.innerWidth;
      VWindowHeight = window.innerHeight;
      var $tmp1 = this.FFormType;
      if ($tmp1 === 0) {
        VWidth = this.FWidth;
        VHeight = this.FHeight;
        VLeft = Math.floor((VWindowWidth - VWidth) / 2);
        VTop = Math.floor((VWindowHeight - VHeight) / 2);
        this.SetBounds(VLeft,VTop,VWidth,VHeight);
      } else if ($tmp1 === 1) {
        this.SetBounds(0,0,VWindowWidth,VWindowHeight);
      };
      this.DoResize();
    };
    this.Show = function () {
      $mod.Application().FActiveForm = this;
      $mod.Application().SetTitle(this.GetText());
      this.BeginUpdate();
      try {
        this.SetVisible(true);
        this.Resize();
      } finally {
        this.EndUpdate();
      };
      this.BringToFront();
      this.SetFocus();
      this.DoShow();
    };
  });
  rtl.createClass($mod,"TApplication",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FForms = null;
      this.FActiveForm = null;
      this.FMainForm = null;
      this.FStopOnException = false;
      this.FTerminated = false;
      this.FTitle = "";
      this.FOnResize = null;
      this.FOnUnload = null;
    };
    this.$final = function () {
      this.FForms = undefined;
      this.FActiveForm = undefined;
      this.FMainForm = undefined;
      this.FOnResize = undefined;
      this.FOnUnload = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetApplicatioName = function () {
      var Result = "";
      Result = window.location.pathname;
      return Result;
    };
    this.GetForm = function (AIndex) {
      var Result = null;
      Result = rtl.getObject(this.FForms[AIndex]);
      return Result;
    };
    this.GetFormCount = function () {
      var Result = 0;
      Result = this.FForms.length;
      return Result;
    };
    this.SetTitle = function (AValue) {
      if (this.FTitle !== AValue) {
        this.FTitle = AValue;
        document.title = this.FTitle;
      };
    };
    this.DoResize = function () {
      if (this.FOnResize != null) {
        this.FOnResize(this);
      };
    };
    this.DoUnload = function () {
      if (this.FOnUnload != null) {
        this.FOnUnload(this);
      };
    };
    this.LoadIcon = function () {
      var $with1 = document.head.appendChild(document.createElement("link"));
      $with1.setAttribute("rel","icon");
      $with1.setAttribute("type","image\/icon");
      $with1.setAttribute("href",this.GetApplicatioName().replace("html","ico"));
    };
    this.RegisterForm = function (AForm) {
      if (AForm != null) {
        if (this.FForms.indexOf(AForm) === -1) {
          this.FForms.push(AForm);
          if (!document.body.contains(AForm.FHandleElement)) {
            document.body.appendChild(AForm.FHandleElement);
          };
        };
      };
    };
    this.UnRegisterForm = function (AForm) {
      var VIndex = 0;
      if (AForm != null) {
        VIndex = this.FForms.indexOf(AForm);
        if (VIndex >= 0) {
          this.FForms.splice(VIndex,1);
          if (document.body.contains(AForm.FHandleElement)) {
            document.body.removeChild(AForm.FHandleElement);
          };
        };
      };
    };
    this.RegisterHandleEvents = function () {
      window.addEventListener("error",rtl.createCallback(this,"HandleError"));
      window.addEventListener("resize",rtl.createCallback(this,"HandleResize"));
      window.addEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    this.UnRegisterHandleEvents = function () {
      window.removeEventListener("error",rtl.createCallback(this,"HandleError"));
      window.removeEventListener("resize",rtl.createCallback(this,"HandleResize"));
      window.removeEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    var CLE = pas.System.LineEnding;
    var CError = (((("Error Message: %s " + CLE) + "Line Nro: %d ") + CLE) + "Column Nro: %d ") + CLE;
    this.HandleError = function (AEvent) {
      var Result = false;
      if (AEvent.message.toLowerCase().indexOf("script error",0) > -1) {
        window.alert("Script Error: See Browser Console for Detail");
      } else {
        window.alert(pas.SysUtils.Format(CError,[AEvent.message,AEvent.lineno,AEvent.colno]));
      };
      if (this.FStopOnException) {
        this.Terminate();
      };
      AEvent.stopPropagation();
      Result = false;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VIndex = 0;
      AEvent.stopPropagation();
      this.DoResize();
      Result = true;
      for (var $l1 = 0, $end2 = this.FForms.length - 1; $l1 <= $end2; $l1++) {
        VIndex = $l1;
        VForm = rtl.getObject(this.FForms[VIndex]);
        if ((VForm != null) && VForm.FVisible) {
          VForm.Resize();
        };
      };
      return Result;
    };
    this.HandleUnload = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      Result = true;
      try {
        this.DoUnload();
      } finally {
        this.Terminate();
      };
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FForms = new Array();
      this.FMainForm = null;
      this.FStopOnException = true;
      this.FTerminated = false;
      this.FTitle = "";
    };
    this.Destroy = function () {
      this.FForms.length = 0;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.CreateForm = function (AInstanceClass, AReference) {
      try {
        AReference.set(AInstanceClass.$create("Create$1",[this]));
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var E = $e;
          AReference.set(null);
        } else throw $e
      };
    };
    this.Initialize = function () {
    };
    this.Run = function () {
      this.RegisterHandleEvents();
      this.LoadIcon();
      if (this.FMainForm != null) {
        this.FMainForm.Show();
      };
    };
    this.Terminate = function () {
      var VForm = null;
      var VIndex = 0;
      if (!this.FTerminated) {
        this.UnRegisterHandleEvents();
        this.FTerminated = true;
        for (var $l1 = this.FForms.length - 1; $l1 >= 0; $l1--) {
          VIndex = $l1;
          VForm = rtl.getObject(this.FForms[VIndex]);
          if (VForm != null) {
            VForm.$destroy("Destroy");
            VForm = null;
          };
        };
      };
    };
    this.UpdateMainForm = function (AForm) {
      if (!(this.FMainForm != null)) {
        this.FMainForm = AForm;
        this.FActiveForm = AForm;
      };
    };
  });
  this.Application = function () {
    var Result = null;
    if (!($impl.VAppInstance != null)) {
      $impl.VAppInstance = $mod.TApplication.$create("Create$1",[null]);
    };
    Result = $impl.VAppInstance;
    return Result;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.VAppInstance = null;
});
rtl.module("DateUtils",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.DateTimeToRFC3339 = function (ADate) {
    var Result = "";
    Result = pas.SysUtils.FormatDateTime('yyyy-mm-dd"T"hh":"nn":"ss"."zzz"Z"',ADate);
    return Result;
  };
  var P = [11,1,6,9,12,15,18];
  this.TryRFC3339ToDateTime = function (Avalue, ADateTime) {
    var Result = false;
    this.TPartPos = {"0": "ppTime", ppTime: 0, "1": "ppYear", ppYear: 1, "2": "ppMonth", ppMonth: 2, "3": "ppDay", ppDay: 3, "4": "ppHour", ppHour: 4, "5": "ppMinute", ppMinute: 5, "6": "ppSec", ppSec: 6};
    var lY = 0;
    var lM = 0;
    var lD = 0;
    var lH = 0;
    var lMi = 0;
    var lS = 0;
    if (pas.SysUtils.Trim(Avalue) === "") {
      Result = true;
      ADateTime.set(0);
    };
    lY = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[1],4),-1);
    lM = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[2],2),-1);
    lD = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[3],2),-1);
    if (Avalue.length >= P[0]) {
      lH = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[4],2),-1);
      lMi = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[5],2),-1);
      lS = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[6],2),-1);
    } else {
      lH = 0;
      lMi = 0;
      lS = 0;
    };
    Result = (((((lY >= 0) && (lM >= 0)) && (lD >= 0)) && (lH >= 0)) && (lMi >= 0)) && (lS >= 0);
    if (!Result) {
      ADateTime.set(0)}
     else if (((lY === 0) || (lM === 0)) || (lD === 0)) {
      ADateTime.set(pas.SysUtils.EncodeTime(lH,lMi,lS,0))}
     else ADateTime.set(pas.SysUtils.EncodeDate(lY,lM,lD) + pas.SysUtils.EncodeTime(lH,lMi,lS,0));
    return Result;
  };
},["JS","RTLConsts"]);
rtl.module("DBConst",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SActiveDataset: {org: "Operation cannot be performed on an active dataset"}, SDuplicateFieldName: {org: 'Duplicate fieldname : "%s"'}, SFieldNotFound: {org: 'Field not found : "%s"'}, SInactiveDataset: {org: "Operation cannot be performed on an inactive dataset"}, SInvalidDisplayValues: {org: '"%s" are not valid boolean displayvalues'}, SInvalidFieldSize: {org: "Invalid field size : %d"}, SInvalidTypeConversion: {org: "Invalid type conversion to %s in field %s"}, SNeedField: {org: "Field %s is required, but not supplied."}, SNeedFieldName: {org: "Field needs a name"}, SNoDataset: {org: 'No dataset asssigned for field : "%s"'}, SNoSuchRecord: {org: "Could not find the requested record."}, SNotEditing: {org: 'Operation not allowed, dataset "%s" is not in an edit or insert state.'}, SRangeError: {org: "%f is not between %f and %f for %s"}, SUniDirectional: {org: "Operation cannot be performed on an unidirectional dataset"}, SUnknownFieldType: {org: "Unknown field type : %s"}, SFieldValueError: {org: "Invalid value for field '%s'"}, SDuplicateName: {org: "Duplicate name '%s' in %s"}, SLookupInfoError: {org: "Lookup information for field '%s' is incomplete"}, SErrInvalidDateTime: {org: 'Invalid date\/time value : "%s"'}, SErrInsertingSameRecordtwice: {org: "Attempt to insert the same record twice."}};
});
rtl.module("DB",["System","Classes","SysUtils","JS","Types","DateUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TDataSetState = {"0": "dsInactive", dsInactive: 0, "1": "dsBrowse", dsBrowse: 1, "2": "dsEdit", dsEdit: 2, "3": "dsInsert", dsInsert: 3, "4": "dsSetKey", dsSetKey: 4, "5": "dsCalcFields", dsCalcFields: 5, "6": "dsFilter", dsFilter: 6, "7": "dsNewValue", dsNewValue: 7, "8": "dsOldValue", dsOldValue: 8, "9": "dsCurValue", dsCurValue: 9, "10": "dsBlockRead", dsBlockRead: 10, "11": "dsInternalCalc", dsInternalCalc: 11, "12": "dsOpening", dsOpening: 12, "13": "dsRefreshFields", dsRefreshFields: 13};
  this.TDataEvent = {"0": "deFieldChange", deFieldChange: 0, "1": "deRecordChange", deRecordChange: 1, "2": "deDataSetChange", deDataSetChange: 2, "3": "deDataSetScroll", deDataSetScroll: 3, "4": "deLayoutChange", deLayoutChange: 4, "5": "deUpdateRecord", deUpdateRecord: 5, "6": "deUpdateState", deUpdateState: 6, "7": "deCheckBrowseMode", deCheckBrowseMode: 7, "8": "dePropertyChange", dePropertyChange: 8, "9": "deFieldListChange", deFieldListChange: 9, "10": "deFocusControl", deFocusControl: 10, "11": "deParentScroll", deParentScroll: 11, "12": "deConnectChange", deConnectChange: 12, "13": "deReconcileError", deReconcileError: 13, "14": "deDisabledStateChange", deDisabledStateChange: 14};
  this.TUpdateStatus = {"0": "usUnmodified", usUnmodified: 0, "1": "usModified", usModified: 1, "2": "usInserted", usInserted: 2, "3": "usDeleted", usDeleted: 3, "4": "usResolved", usResolved: 4, "5": "usResolveFailed", usResolveFailed: 5};
  this.TProviderFlag = {"0": "pfInUpdate", pfInUpdate: 0, "1": "pfInWhere", pfInWhere: 1, "2": "pfInKey", pfInKey: 2, "3": "pfHidden", pfHidden: 3, "4": "pfRefreshOnInsert", pfRefreshOnInsert: 4, "5": "pfRefreshOnUpdate", pfRefreshOnUpdate: 5};
  $mod.$rtti.$Enum("TProviderFlag",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TProviderFlag});
  $mod.$rtti.$Set("TProviderFlags",{comptype: $mod.$rtti["TProviderFlag"]});
  $mod.$rtti.$Class("TField");
  $mod.$rtti.$Class("TDataSet");
  $mod.$rtti.$Class("TDataSource");
  rtl.createClass($mod,"EDatabaseError",pas.SysUtils.Exception,function () {
  });
  this.TFieldType = {"0": "ftUnknown", ftUnknown: 0, "1": "ftString", ftString: 1, "2": "ftInteger", ftInteger: 2, "3": "ftLargeInt", ftLargeInt: 3, "4": "ftBoolean", ftBoolean: 4, "5": "ftFloat", ftFloat: 5, "6": "ftDate", ftDate: 6, "7": "ftTime", ftTime: 7, "8": "ftDateTime", ftDateTime: 8, "9": "ftAutoInc", ftAutoInc: 9, "10": "ftBlob", ftBlob: 10, "11": "ftMemo", ftMemo: 11, "12": "ftFixedChar", ftFixedChar: 12, "13": "ftVariant", ftVariant: 13, "14": "ftDataset", ftDataset: 14};
  $mod.$rtti.$Enum("TFieldType",{minvalue: 0, maxvalue: 14, ordtype: 1, enumtype: this.TFieldType});
  this.TFieldAttribute = {"0": "faHiddenCol", faHiddenCol: 0, "1": "faReadonly", faReadonly: 1, "2": "faRequired", faRequired: 2, "3": "faLink", faLink: 3, "4": "faUnNamed", faUnNamed: 4, "5": "faFixed", faFixed: 5};
  $mod.$rtti.$Enum("TFieldAttribute",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TFieldAttribute});
  $mod.$rtti.$Set("TFieldAttributes",{comptype: $mod.$rtti["TFieldAttribute"]});
  rtl.createClass($mod,"TNamedItem",pas.Classes.TCollectionItem,function () {
    this.$init = function () {
      pas.Classes.TCollectionItem.$init.call(this);
      this.FName = "";
    };
    this.GetDisplayName = function () {
      var Result = "";
      Result = this.FName;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      var TmpInd = 0;
      if (this.FName === Value) return;
      if ((Value !== "") && $mod.TFieldDefs.isPrototypeOf(this.FCollection)) {
        TmpInd = this.FCollection.IndexOf(Value);
        if ((TmpInd >= 0) && (TmpInd !== this.GetIndex())) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SDuplicateName"),[Value,this.FCollection.$classname]);
      };
      this.FName = Value;
      pas.Classes.TCollectionItem.SetDisplayName.call(this,Value);
    };
    var $r = this.$rtti;
    $r.addProperty("Name",2,rtl.string,"FName","SetDisplayName");
  });
  rtl.createClass($mod,"TDefCollection",pas.Classes.TOwnedCollection,function () {
    this.$init = function () {
      pas.Classes.TOwnedCollection.$init.call(this);
      this.FDataset = null;
    };
    this.$final = function () {
      this.FDataset = undefined;
      pas.Classes.TOwnedCollection.$final.call(this);
    };
    this.SetItemName = function (Item) {
      var N = null;
      var TN = "";
      N = rtl.as(Item,$mod.TNamedItem);
      if (N.FName === "") {
        TN = pas.System.Copy(this.$classname,2,5) + pas.SysUtils.IntToStr(N.FID + 1);
        if (this.FDataset != null) TN = this.FDataset.FName + TN;
        N.SetDisplayName(TN);
      } else pas.Classes.TCollection.SetItemName.call(this,Item);
    };
    this.create$3 = function (ADataset, AOwner, AClass) {
      pas.Classes.TOwnedCollection.Create$2.call(this,AOwner,AClass);
      this.FDataset = ADataset;
    };
    this.IndexOf = function (AName) {
      var Result = 0;
      var i = 0;
      Result = -1;
      for (var $l1 = 0, $end2 = this.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (pas.SysUtils.AnsiSameText(this.GetItem(i).FName,AName)) {
          Result = i;
          break;
        };
      };
      return Result;
    };
  });
  rtl.createClass($mod,"TFieldDef",$mod.TNamedItem,function () {
    this.$init = function () {
      $mod.TNamedItem.$init.call(this);
      this.FAttributes = {};
      this.FDataType = 0;
      this.FFieldNo = 0;
      this.FInternalCalcField = false;
      this.FPrecision = 0;
      this.FRequired = false;
      this.FSize = 0;
    };
    this.$final = function () {
      this.FAttributes = undefined;
      $mod.TNamedItem.$final.call(this);
    };
    this.GetFieldClass = function () {
      var Result = null;
      if (((this.FCollection != null) && $mod.TFieldDefs.isPrototypeOf(this.FCollection)) && (this.FCollection.FDataset != null)) {
        Result = this.FCollection.FDataset.GetFieldClass(this.FDataType)}
       else Result = null;
      return Result;
    };
    this.SetAttributes = function (AValue) {
      this.FAttributes = rtl.refSet(AValue);
      this.Changed(false);
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
      this.Changed(false);
    };
    this.SetPrecision = function (AValue) {
      this.FPrecision = AValue;
      this.Changed(false);
    };
    this.SetSize = function (AValue) {
      this.FSize = AValue;
      this.Changed(false);
    };
    this.Create$1 = function (ACollection) {
      pas.Classes.TCollectionItem.Create$1.call(this,ACollection);
      this.FFieldNo = this.GetIndex() + 1;
    };
    this.Create$3 = function (AOwner, AName, ADataType, ASize, ARequired, AFieldNo) {
      pas.Classes.TCollectionItem.Create$1.call(this,AOwner);
      this.SetDisplayName(AName);
      this.FDataType = ADataType;
      this.FSize = ASize;
      this.FRequired = ARequired;
      this.FPrecision = -1;
      this.FFieldNo = AFieldNo;
    };
    this.Destroy = function () {
      pas.Classes.TCollectionItem.Destroy.call(this);
    };
    this.Assign = function (Source) {
      var fd = null;
      fd = null;
      if ($mod.TFieldDef.isPrototypeOf(Source)) fd = rtl.as(Source,$mod.TFieldDef);
      if (fd != null) {
        this.FCollection.BeginUpdate();
        try {
          this.SetDisplayName(fd.FName);
          this.SetDataType(fd.FDataType);
          this.SetSize(fd.FSize);
          this.SetPrecision(fd.FPrecision);
          this.FRequired = fd.FRequired;
        } finally {
          this.FCollection.EndUpdate();
        };
      } else pas.Classes.TPersistent.Assign.call(this,Source);
    };
    this.CreateField = function (AOwner) {
      var Result = null;
      var TheField = null;
      TheField = this.GetFieldClass();
      if (TheField === null) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SUnknownFieldType"),[this.FName]);
      Result = TheField.$create("Create$1",[AOwner]);
      try {
        Result.FFieldDef = this;
        Result.SetSize(this.FSize);
        Result.FRequired = this.FRequired;
        Result.FFieldName = this.FName;
        Result.FDisplayLabel = this.GetDisplayName();
        Result.FFieldNo = this.FFieldNo;
        Result.SetFieldType(this.FDataType);
        Result.FReadOnly = 1 in this.FAttributes;
        Result.SetDataset(this.FCollection.FDataset);
        if ($mod.TFloatField.isPrototypeOf(Result)) Result.SetPrecision(this.FPrecision);
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Attributes",2,$mod.$rtti["TFieldAttributes"],"FAttributes","SetAttributes",{Default: {}});
    $r.addProperty("DataType",2,$mod.$rtti["TFieldType"],"FDataType","SetDataType");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 0});
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass($mod,"TFieldDefs",$mod.TDefCollection,function () {
    this.GetItem$1 = function (Index) {
      var Result = null;
      Result = this.GetItem(Index);
      return Result;
    };
    this.FieldDefClass = function () {
      var Result = null;
      Result = $mod.TFieldDef;
      return Result;
    };
    this.Create$4 = function (ADataSet) {
      $mod.TDefCollection.create$3.call(this,ADataSet,this.Owner(),this.$class.FieldDefClass());
    };
    this.Add$2 = function (AName, ADataType, ASize, ARequired, AFieldNo) {
      var Result = null;
      Result = this.$class.FieldDefClass().$create("Create$3",[this,AName,ADataType,ASize,ARequired,AFieldNo]);
      return Result;
    };
    this.Add$3 = function (AName, ADataType, ASize, ARequired) {
      if (AName.length === 0) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SNeedFieldName"),this.FDataset);
      this.BeginUpdate();
      try {
        this.Add$2(AName,ADataType,ASize,ARequired,this.GetCount() + 1);
      } finally {
        this.EndUpdate();
      };
    };
    this.Add$4 = function (AName, ADataType, ASize) {
      this.Add$3(AName,ADataType,ASize,false);
    };
  });
  this.TFieldKind = {"0": "fkData", fkData: 0, "1": "fkCalculated", fkCalculated: 1, "2": "fkLookup", fkLookup: 2, "3": "fkInternalCalc", fkInternalCalc: 3};
  $mod.$rtti.$Enum("TFieldKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFieldKind});
  $mod.$rtti.$MethodVar("TFieldNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TFieldGetTextEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]],["aText",rtl.string,1],["DisplayText",rtl.boolean]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TFieldSetTextEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]],["aText",rtl.string,2]]), methodkind: 0});
  rtl.createClass($mod,"TLookupList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function () {
      this.FList = pas.Classes.TFPList.$create("Create");
    };
    this.Destroy = function () {
      this.Clear();
      this.FList.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      this.FList.Clear();
    };
  });
  rtl.createClass($mod,"TField",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAlignment = 0;
      this.FConstraintErrorMessage = "";
      this.FCustomConstraint = "";
      this.FDataSet = null;
      this.FDataType = 0;
      this.FDefaultExpression = "";
      this.FDisplayLabel = "";
      this.FDisplayWidth = 0;
      this.FFieldDef = null;
      this.FFieldKind = 0;
      this.FFieldName = "";
      this.FFieldNo = 0;
      this.FFields = null;
      this.FHasConstraints = false;
      this.FImportedConstraint = "";
      this.FKeyFields = "";
      this.FLookupCache = false;
      this.FLookupDataSet = null;
      this.FLookupKeyfields = "";
      this.FLookupresultField = "";
      this.FLookupList = null;
      this.FOnChange = null;
      this.FOnGetText = null;
      this.FOnSetText = null;
      this.FOnValidate = null;
      this.FOrigin = "";
      this.FReadOnly = false;
      this.FRequired = false;
      this.FSize = 0;
      this.FValidChars = [];
      this.FValueBuffer = undefined;
      this.FValidating = false;
      this.FVisible = false;
      this.FProviderFlags = {};
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FFieldDef = undefined;
      this.FFields = undefined;
      this.FLookupDataSet = undefined;
      this.FLookupList = undefined;
      this.FOnChange = undefined;
      this.FOnGetText = undefined;
      this.FOnSetText = undefined;
      this.FOnValidate = undefined;
      this.FValidChars = undefined;
      this.FProviderFlags = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FDataSet != null) {
        Result = this.FDataSet.FFieldList.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.PropertyChanged(false);
      };
    };
    this.SetIndex = function (AValue) {
      if (this.FFields !== null) this.FFields.SetFieldIndex(this,AValue);
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = rtl.strSetLength(Result,0);
      if (this.FOnGetText != null) {
        this.FOnGetText(this,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},true)}
       else this.GetText({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},true);
      return Result;
    };
    this.SetDisplayLabel = function (AValue) {
      if (this.FDisplayLabel !== AValue) {
        this.FDisplayLabel = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetDisplayWidth = function (AValue) {
      if (this.FDisplayWidth !== AValue) {
        this.FDisplayWidth = AValue;
        this.PropertyChanged(true);
      };
    };
    this.GetDisplayWidth = function () {
      var Result = 0;
      if (this.FDisplayWidth === 0) {
        Result = this.GetDefaultWidth()}
       else Result = this.FDisplayWidth;
      return Result;
    };
    this.SetReadOnly = function (AValue) {
      if (this.FReadOnly !== AValue) {
        this.FReadOnly = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.PropertyChanged(true);
      };
    };
    this.IsDisplayLabelStored = function () {
      var Result = false;
      Result = this.GetDisplayName() !== this.FFieldName;
      return Result;
    };
    this.IsDisplayWidthStored = function () {
      var Result = false;
      Result = this.FDisplayWidth !== 0;
      return Result;
    };
    this.GetLookupList = function () {
      var Result = null;
      if (!(this.FLookupList != null)) this.FLookupList = $mod.TLookupList.$create("Create$1");
      Result = this.FLookupList;
      return Result;
    };
    this.CalcLookupValue = function () {
    };
    this.RaiseAccessError = function (TypeName) {
      var E = null;
      E = this.AccessError(TypeName);
      throw E;
    };
    this.AccessError = function (TypeName) {
      var Result = null;
      Result = $mod.EDatabaseError.$create("CreateFmt",[rtl.getResStr(pas.DBConst,"SInvalidTypeConversion"),[TypeName,this.FFieldName]]);
      return Result;
    };
    this.CheckInactive = function () {
      if (this.FDataSet != null) this.FDataSet.CheckInactive();
    };
    this.CheckTypeSize = function (AValue) {
      if ((AValue !== 0) && !this.IsBlob()) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.Change = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Bind = function (Binding) {
      if (Binding && (this.FFieldKind === 2)) {
        if ((((this.FLookupDataSet === null) || (this.FLookupKeyfields === "")) || (this.FLookupresultField === "")) || (this.FKeyFields === "")) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SLookupInfoError"),[this.GetDisplayName()]);
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.Open();
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        if (this.FLookupCache) this.RefreshLookupList();
      };
    };
    this.GetAsBytes = function () {
      var Result = [];
      this.RaiseAccessError($impl.SBytes);
      Result = [];
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      Result = this.GetClassDesc();
      return Result;
    };
    this.GetClassDesc = function () {
      var Result = "";
      var ClassN = "";
      ClassN = pas.System.Copy(this.$classname,2,pas.System.Pos("Field",this.$classname) - 2);
      if (this.GetIsNull()) {
        Result = ("(" + pas.SysUtils.LowerCase(ClassN)) + ")"}
       else Result = ("(" + pas.SysUtils.UpperCase(ClassN)) + ")";
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = 10;
      return Result;
    };
    this.GetDisplayName = function () {
      var Result = "";
      if (this.FDisplayLabel !== "") {
        Result = this.FDisplayLabel}
       else Result = this.FFieldName;
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      Result = pas.JS.isNull(this.GetData());
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set(this.GetAsString());
    };
    this.Notification = function (AComponent, Operation) {
      pas.Classes.TComponent.Notification.call(this,AComponent,Operation);
      if ((Operation === 1) && (AComponent === this.FLookupDataSet)) this.FLookupDataSet = null;
    };
    this.PropertyChanged = function (LayoutAffected) {
      if ((this.FDataSet !== null) && this.FDataSet.GetActive()) if (LayoutAffected) {
        this.FDataSet.DataEvent(4,0)}
       else this.FDataSet.DataEvent(2,0);
    };
    this.SetAsJSValue = function (AValue) {
      if (pas.JS.isNull(AValue)) {
        this.Clear()}
       else try {
        this.SetVarValue(AValue);
      } catch ($e) {
        if (pas.SysUtils.EVariantError.isPrototypeOf($e)) {
          $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SFieldValueError"),[this.GetDisplayName()])}
         else throw $e
      };
    };
    this.SetDataset = function (AValue) {
      if (AValue === this.FDataSet) return;
      if (this.FDataSet != null) {
        this.FDataSet.CheckInactive();
        this.FDataSet.FFieldList.Remove(this);
      };
      if (AValue != null) {
        AValue.CheckInactive();
        AValue.FFieldList.Add(this);
      };
      this.FDataSet = AValue;
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
    };
    this.SetSize = function (AValue) {
      this.CheckInactive();
      this.$class.CheckTypeSize(AValue);
      this.FSize = AValue;
    };
    this.SetVarValue = function (AValue) {
      this.RaiseAccessError($impl.SJSValue);
    };
    this.SetAsBytes = function (AValue) {
      this.RaiseAccessError($impl.SBytes);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FVisible = true;
      this.FValidChars = rtl.arraySetLength(this.FValidChars,"",255);
      this.FProviderFlags = rtl.createSet(0,1);
    };
    this.Destroy = function () {
      if (this.FDataSet != null) {
        this.FDataSet.SetActive(false);
        if (this.FFields != null) this.FFields.Remove(this);
      };
      rtl.free(this,"FLookupList");
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.Assign = function (Source) {
      if (Source === null) {
        this.Clear()}
       else if ($mod.TField.isPrototypeOf(Source)) {
        this.SetAsJSValue(Source.GetAsJSValue());
      } else pas.Classes.TPersistent.Assign.call(this,Source);
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.GetData = function () {
      var Result = undefined;
      if (this.FDataSet === null) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNoDataset"),[this.FFieldName]);
      if (this.FValidating) {
        Result = this.FValueBuffer}
       else Result = this.FDataSet.GetFieldData(this);
      return Result;
    };
    this.IsBlob = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.RefreshLookupList = function () {
      var tmpActive = false;
      if (((!(this.FLookupDataSet != null) || (this.FLookupKeyfields.length === 0)) || (this.FLookupresultField.length === 0)) || (this.FKeyFields.length === 0)) return;
      tmpActive = this.FLookupDataSet.GetActive();
      try {
        this.FLookupDataSet.SetActive(true);
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        this.GetLookupList().Clear();
        this.FLookupDataSet.DisableControls();
        try {
          this.FLookupDataSet.First();
          while (!this.FLookupDataSet.FEOF) {
            this.FLookupDataSet.Next();
          };
        } finally {
          this.FLookupDataSet.EnableControls();
        };
      } finally {
        this.FLookupDataSet.SetActive(tmpActive);
      };
    };
    this.SetData = function (Buffer) {
      if (!(this.FDataSet != null)) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNoDataset"),[this.FFieldName]);
      this.FDataSet.SetFieldData(this,Buffer);
    };
    this.SetFieldType = function (AValue) {
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taLeftJustify});
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ConstraintErrorMessage",0,rtl.string,"FConstraintErrorMessage","FConstraintErrorMessage");
    $r.addProperty("DefaultExpression",0,rtl.string,"FDefaultExpression","FDefaultExpression");
    $r.addProperty("DisplayLabel",15,rtl.string,"GetDisplayName","SetDisplayLabel",{stored: "IsDisplayLabelStored"});
    $r.addProperty("DisplayWidth",15,rtl.longint,"GetDisplayWidth","SetDisplayWidth",{stored: "IsDisplayWidthStored"});
    $r.addProperty("FieldKind",0,$mod.$rtti["TFieldKind"],"FFieldKind","FFieldKind");
    $r.addProperty("FieldName",0,rtl.string,"FFieldName","FFieldName");
    $r.addProperty("HasConstraints",0,rtl.boolean,"FHasConstraints","");
    $r.addProperty("Index",3,rtl.longint,"GetIndex","SetIndex");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
    $r.addProperty("KeyFields",0,rtl.string,"FKeyFields","FKeyFields");
    $r.addProperty("LookupCache",0,rtl.boolean,"FLookupCache","FLookupCache");
    $r.addProperty("LookupDataSet",0,$mod.$rtti["TDataSet"],"FLookupDataSet","FLookupDataSet");
    $r.addProperty("LookupKeyFields",0,rtl.string,"FLookupKeyfields","FLookupKeyfields");
    $r.addProperty("LookupResultField",0,rtl.string,"FLookupresultField","FLookupresultField");
    $r.addProperty("Origin",0,rtl.string,"FOrigin","FOrigin");
    $r.addProperty("ProviderFlags",0,$mod.$rtti["TProviderFlags"],"FProviderFlags","FProviderFlags");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("Required",0,rtl.boolean,"FRequired","FRequired");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible",{Default: true});
    $r.addProperty("OnChange",0,$mod.$rtti["TFieldNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnGetText",0,$mod.$rtti["TFieldGetTextEvent"],"FOnGetText","FOnGetText");
    $r.addProperty("OnSetText",0,$mod.$rtti["TFieldSetTextEvent"],"FOnSetText","FOnSetText");
    $r.addProperty("OnValidate",0,$mod.$rtti["TFieldNotifyEvent"],"FOnValidate","FOnValidate");
  });
  rtl.createClass($mod,"TStringField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FFixedChar = false;
      this.FTransliterate = false;
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue < 0) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if (rtl.isString(V)) {
        Result = "" + V}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FSize;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set(this.GetAsString());
    };
    this.SetAsString = function (AValue) {
      this.SetData(AValue);
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError(rtl.getResStr(pas.DBConst,"SFieldValueError"));
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType(1);
      this.FFixedChar = false;
      this.FTransliterate = false;
      this.FSize = 20;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in rtl.createSet(1,12)) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 20});
  });
  rtl.createClass($mod,"TNumericField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayFormat = "";
      this.FEditFormat = "";
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue > 16) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.RangeError = function (AValue, Min, Max) {
      $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SRangeError"),[AValue,Min,Max,this.FFieldName]);
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetEditFormat = function (AValue) {
      if (this.FEditFormat !== AValue) {
        this.FEditFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetAlignment(1);
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taRightJustify});
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
    $r.addProperty("EditFormat",2,rtl.string,"FEditFormat","SetEditFormat");
  });
  rtl.createClass($mod,"TIntegerField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = pas.SysUtils.IntToStr(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var l = 0;
      var fmt = "";
      AText.set("");
      if (!this.GetValue({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }})) return;
      if (ADisplayText || (this.FEditFormat === "")) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      if (fmt.length !== 0) {
        AText.set(pas.SysUtils.FormatFloat(fmt,l))}
       else AText.set("" + l);
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var V = undefined;
      V = this.GetData();
      Result = pas.JS.isInteger(V);
      if (Result) AValue.set(Math.floor(V));
      return Result;
    };
    this.SetAsInteger = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        this.RangeError(AValue,this.FMinValue,this.FMaxValue)}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isInteger(AValue)) {
        this.SetAsInteger(Math.floor(AValue))}
       else this.RaiseAccessError($impl.SInteger);
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType(2);
      this.FMinRange = -2147483648;
      this.FMaxRange = 2147483647;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.longint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.longint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass($mod,"TLargeintField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = pas.SysUtils.IntToStr(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var l = 0;
      var fmt = "";
      AText.set("");
      if (!this.GetValue({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }})) return;
      if (ADisplayText || (this.FEditFormat === "")) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      if (fmt.length !== 0) {
        AText.set(pas.SysUtils.FormatFloat(fmt,l))}
       else AText.set("" + l);
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var P = undefined;
      P = this.GetData();
      Result = pas.JS.isInteger(P);
      if (Result) AValue.set(Math.floor(P));
      return Result;
    };
    this.SetAsLargeInt = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.RangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isInteger(AValue)) {
        this.SetAsLargeInt(Math.floor(AValue))}
       else this.RaiseAccessError($impl.SLargeInt);
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType(3);
      this.FMinRange = -4503599627370496;
      this.FMaxRange = 4503599627370495;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.nativeint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.nativeint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass($mod,"TAutoIncField",$mod.TIntegerField,function () {
    this.SetAsInteger = function (AValue) {
      $mod.TIntegerField.SetAsInteger.apply(this,arguments);
    };
    this.Create$1 = function (AOwner) {
      $mod.TIntegerField.Create$1.call(this,AOwner);
      this.SetDataType(9);
    };
  });
  rtl.createClass($mod,"TFloatField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FCurrency = false;
      this.FMaxValue = 0.0;
      this.FMinValue = 0.0;
      this.FPrecision = 0;
    };
    this.SetCurrency = function (AValue) {
      if (this.FCurrency === AValue) return;
      this.FCurrency = AValue;
    };
    this.SetPrecision = function (AValue) {
      if ((AValue === -1) || (AValue > 1)) {
        this.FPrecision = AValue}
       else this.FPrecision = 2;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = P}
       else Result = null;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = pas.SysUtils.FloatToStr(rtl.getNumber(P))}
       else Result = "";
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var fmt = "";
      var E = 0.0;
      var Digits = 0;
      var ff = 0;
      var P = undefined;
      AText.set("");
      P = this.GetData();
      if (!rtl.isNumber(P)) return;
      E = rtl.getNumber(P);
      if (ADisplayText || (this.FEditFormat.length === 0)) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      Digits = 0;
      if (!this.FCurrency) {
        ff = 1}
       else {
        Digits = 2;
        ff = 0;
      };
      if (fmt !== "") {
        AText.set(pas.SysUtils.FormatFloat(fmt,E))}
       else AText.set(pas.SysUtils.FloatToStrF(E,ff,this.FPrecision,Digits));
    };
    this.SetAsFloat = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.RangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isNumber(AValue)) {
        this.SetAsFloat(rtl.getNumber(AValue))}
       else this.RaiseAccessError("Float");
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType(5);
      this.FPrecision = 15;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = true;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Currency",2,rtl.boolean,"FCurrency","SetCurrency",{Default: false});
    $r.addProperty("MaxValue",0,rtl.double,"FMaxValue","FMaxValue");
    $r.addProperty("MinValue",0,rtl.double,"FMinValue","FMinValue");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 15});
  });
  rtl.createClass($mod,"TBooleanField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayValues = "";
      this.FDisplays = rtl.arraySetLength(null,"",2,2);
    };
    this.$final = function () {
      this.FDisplays = undefined;
      $mod.TField.$final.call(this);
    };
    this.SetDisplayValues = function (AValue) {
      var I = 0;
      if (this.FDisplayValues !== AValue) {
        I = pas.System.Pos(";",AValue);
        if ((I < 2) || (I === AValue.length)) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidDisplayValues"),[AValue]);
        this.FDisplayValues = AValue;
        this.FDisplays[0][1] = pas.System.Copy(AValue,1,I - 1);
        this.FDisplays[1][1] = pas.SysUtils.UpperCase(this.FDisplays[0][1]);
        this.FDisplays[0][0] = pas.System.Copy(AValue,I + 1,AValue.length - I);
        this.FDisplays[1][0] = pas.SysUtils.UpperCase(this.FDisplays[0][0]);
        this.PropertyChanged(true);
      };
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if (pas.JS.isBoolean(P)) {
        Result = this.FDisplays[0][+!(P == false)]}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if (pas.JS.isBoolean(P)) {
        Result = !(P == false)}
       else Result = null;
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FDisplays[0][0].length;
      if (Result < this.FDisplays[0][1].length) Result = this.FDisplays[0][1].length;
      return Result;
    };
    this.SetAsBoolean = function (AValue) {
      this.SetData(AValue);
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isBoolean(AValue)) {
        this.SetAsBoolean(!(AValue == false))}
       else if (rtl.isNumber(AValue)) this.SetAsBoolean(rtl.getNumber(AValue) !== 0);
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType(4);
      this.SetDisplayValues("True;False");
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayValues",2,rtl.string,"FDisplayValues","SetDisplayValues");
  });
  rtl.createClass($mod,"TDateTimeField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayFormat = "";
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.ConvertToDateTime = function (aValue, aRaiseError) {
      var Result = 0.0;
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertToDateTime(aValue,aRaiseError)}
       else Result = $mod.TDataSet.DefaultConvertToDateTime(aValue,aRaiseError);
      return Result;
    };
    this.DateTimeToNativeDateTime = function (aValue) {
      var Result = undefined;
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertDateTimeToNative(aValue)}
       else Result = $mod.TDataSet.DefaultConvertDateTimeToNative(aValue);
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      this.GetText({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},false);
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      if (!rtl.isString(Result)) Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var R = 0.0;
      var F = "";
      R = this.ConvertToDateTime(this.GetData(),false);
      if (R === 0) {
        AText.set("")}
       else {
        if (ADisplayText && (this.FDisplayFormat.length !== 0)) {
          F = this.FDisplayFormat}
         else {
          var $tmp1 = this.FDataType;
          if ($tmp1 === 7) {
            F = pas.SysUtils.LongTimeFormat}
           else if ($tmp1 === 6) {
            F = pas.SysUtils.ShortDateFormat}
           else {
            F = "c";
          };
        };
        AText.set(pas.SysUtils.FormatDateTime(F,R));
      };
    };
    this.SetAsDateTime = function (AValue) {
      this.SetData(this.DateTimeToNativeDateTime(AValue));
    };
    this.SetVarValue = function (AValue) {
      this.SetAsDateTime(this.ConvertToDateTime(AValue,true));
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType(8);
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
  });
  rtl.createClass($mod,"TDateField",$mod.TDateTimeField,function () {
    this.Create$1 = function (AOwner) {
      $mod.TDateTimeField.Create$1.call(this,AOwner);
      this.SetDataType(6);
    };
  });
  rtl.createClass($mod,"TTimeField",$mod.TDateTimeField,function () {
    this.Create$1 = function (AOwner) {
      $mod.TDateTimeField.Create$1.call(this,AOwner);
      this.SetDataType(7);
    };
  });
  rtl.createClass($mod,"TBinaryField",$mod.TField,function () {
    this.CheckTypeSize = function (AValue) {
      if (AValue < 1) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.BlobToBytes = function (aValue) {
      var Result = [];
      if (this.FDataSet != null) {
        Result = this.FDataSet.BlobDataToBytes(aValue)}
       else Result = $mod.TDataSet.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      var S = [];
      var I = 0;
      Result = "";
      V = this.GetData();
      if (V != null) {
        S = this.BlobToBytes(V);
        for (var $l1 = 0, $end2 = rtl.length(S); $l1 <= $end2; $l1++) {
          I = $l1;
          Result.concat(String.fromCharCode(S[I]));
        };
      };
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetAsString = function (AValue) {
      var B = [];
      var i = 0;
      B = rtl.arraySetLength(B,0,AValue.length);
      for (var $l1 = 1, $end2 = AValue.length; $l1 <= $end2; $l1++) {
        i = $l1;
        B[i - 1] = AValue.charCodeAt(i - 1);
      };
      this.SetAsBytes(B);
    };
    this.SetVarValue = function (AValue) {
      var B = [];
      var I = 0;
      var Len = 0;
      if (rtl.isArray(AValue)) {
        Len = rtl.length(AValue);
        B = rtl.arraySetLength(B,0,Len);
        for (var $l1 = 1, $end2 = Len - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          B[I] = AValue[I];
        };
        this.SetAsBytes(B);
      } else if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError("Blob");
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 16});
  });
  rtl.createClass($mod,"TBlobField",$mod.TBinaryField,function () {
    this.$init = function () {
      $mod.TBinaryField.$init.call(this);
      this.FModified = false;
    };
    this.GetBlobSize = function () {
      var Result = 0;
      var B = [];
      B = this.GetAsBytes();
      Result = rtl.length(B);
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      if (!this.FModified) {
        Result = $mod.TField.GetIsNull.call(this)}
       else Result = this.GetBlobSize() === 0;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set($mod.TBinaryField.GetAsString.call(this));
    };
    this.Create$1 = function (AOwner) {
      $mod.TBinaryField.Create$1.call(this,AOwner);
      this.SetDataType(10);
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.IsBlob = function () {
      var Result = false;
      Result = true;
      return Result;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in $mod.ftBlobTypes) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass($mod,"TMemoField",$mod.TBlobField,function () {
    this.Create$1 = function (AOwner) {
      $mod.TBlobField.Create$1.call(this,AOwner);
      this.SetDataType(11);
    };
  });
  rtl.createClass($mod,"TVariantField",$mod.TField,function () {
    this.CheckTypeSize = function (aValue) {
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if (pas.JS.isInteger(V)) {
        Result = pas.SysUtils.IntToStr(Math.floor(V))}
       else if (rtl.isNumber(V)) {
        Result = pas.SysUtils.FloatToStr(rtl.getNumber(V))}
       else if (rtl.isString(V)) {
        Result = "" + V}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetVarValue = function (aValue) {
      this.SetData(aValue);
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType(13);
    };
  });
  rtl.createClass($mod,"TCheckConstraint",pas.Classes.TCollectionItem,function () {
    this.$init = function () {
      pas.Classes.TCollectionItem.$init.call(this);
      this.FCustomConstraint = "";
      this.FErrorMessage = "";
      this.FFromDictionary = false;
      this.FImportedConstraint = "";
    };
    this.Assign = function (Source) {
    };
    var $r = this.$rtti;
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ErrorMessage",0,rtl.string,"FErrorMessage","FErrorMessage");
    $r.addProperty("FromDictionary",0,rtl.boolean,"FFromDictionary","FFromDictionary");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
  });
  rtl.createClass($mod,"TCheckConstraints",pas.Classes.TCollection,function () {
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Create$2 = function (AOwner) {
      pas.Classes.TCollection.Create$1.call(this,$mod.TCheckConstraint);
    };
  });
  rtl.createClass($mod,"TFields",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FDataset = null;
      this.FFieldList = null;
      this.FOnChange = null;
      this.FValidFieldKinds = {};
    };
    this.$final = function () {
      this.FDataset = undefined;
      this.FFieldList = undefined;
      this.FOnChange = undefined;
      this.FValidFieldKinds = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.ClearFieldDefs = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetField(i).FFieldDef = null;
      };
    };
    this.Changed = function () {
      if ((this.FDataset !== null) && !(3 in this.FDataset.FComponentState)) this.FDataset.DataEvent(9,0);
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FFieldList.FCount;
      return Result;
    };
    this.GetField = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FFieldList.Get(Index));
      return Result;
    };
    this.SetFieldIndex = function (Field, Value) {
      var Old = 0;
      Old = this.FFieldList.IndexOf(Field);
      if (Old === -1) return;
      if (Value < 0) Value = 0;
      if (Value >= this.GetCount()) Value = this.GetCount() - 1;
      if (Value !== Old) {
        this.FFieldList.Delete(Old);
        this.FFieldList.Insert(Value,Field);
        Field.PropertyChanged(true);
        this.Changed();
      };
    };
    this.Create$1 = function (ADataset) {
      this.FDataset = ADataset;
      this.FFieldList = pas.Classes.TFPList.$create("Create");
      this.FValidFieldKinds = rtl.createSet(null,0,3);
    };
    this.Destroy = function () {
      if (this.FFieldList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldList;
        }, set: function (v) {
          this.p.FFieldList = v;
        }});
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Field) {
      this.CheckFieldName(Field.FFieldName);
      this.FFieldList.Add(Field);
      Field.FFields = this;
      this.Changed();
    };
    this.CheckFieldName = function (Value) {
      if (this.FindField(Value) !== null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SDuplicateFieldName"),[Value],this.FDataset);
    };
    this.CheckFieldNames = function (Value) {
      var N = "";
      var StrPos = 0;
      if (Value === "") return;
      StrPos = 1;
      do {
        N = $mod.ExtractFieldName(Value,{get: function () {
            return StrPos;
          }, set: function (v) {
            StrPos = v;
          }});
        this.FieldByName(N);
      } while (!(StrPos > Value.length));
    };
    this.Clear = function () {
      var AField = null;
      while (this.FFieldList.FCount > 0) {
        AField = rtl.getObject(this.FFieldList.Last());
        AField.FDataSet = null;
        AField = rtl.freeLoc(AField);
        this.FFieldList.Delete(this.FFieldList.FCount - 1);
      };
      this.Changed();
    };
    this.FindField = function (Value) {
      var Result = null;
      var S = "";
      var I = 0;
      S = pas.SysUtils.UpperCase(Value);
      for (var $l1 = 0, $end2 = this.FFieldList.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        Result = rtl.getObject(this.FFieldList.Get(I));
        if (S === pas.SysUtils.UpperCase(Result.FFieldName)) {
          return Result;
        };
      };
      Result = null;
      return Result;
    };
    this.FieldByName = function (Value) {
      var Result = null;
      Result = this.FindField(Value);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[Value],this.FDataset);
      return Result;
    };
    this.IndexOf = function (Field) {
      var Result = 0;
      Result = this.FFieldList.IndexOf(Field);
      return Result;
    };
    this.Remove = function (Value) {
      this.FFieldList.Remove(Value);
      Value.FFields = null;
      this.Changed();
    };
  });
  this.TBookmarkFlag = {"0": "bfCurrent", bfCurrent: 0, "1": "bfBOF", bfBOF: 1, "2": "bfEOF", bfEOF: 2, "3": "bfInserted", bfInserted: 3};
  this.TBookmark = function (s) {
    if (s) {
      this.Data = s.Data;
    } else {
      this.Data = undefined;
    };
    this.$equal = function (b) {
      return this.Data === b.Data;
    };
  };
  this.TGetMode = {"0": "gmCurrent", gmCurrent: 0, "1": "gmNext", gmNext: 1, "2": "gmPrior", gmPrior: 2};
  this.TGetResult = {"0": "grOK", grOK: 0, "1": "grBOF", grBOF: 1, "2": "grEOF", grEOF: 2, "3": "grError", grError: 3};
  this.TResyncMode$a = {"0": "rmExact", rmExact: 0, "1": "rmCenter", rmCenter: 1};
  this.TDataAction = {"0": "daFail", daFail: 0, "1": "daAbort", daAbort: 1, "2": "daRetry", daRetry: 2};
  this.TLoadOption = {"0": "loNoOpen", loNoOpen: 0, "1": "loNoEvents", loNoEvents: 1, "2": "loAtEOF", loAtEOF: 2};
  this.TRecordState = {"0": "rsNew", rsNew: 0, "1": "rsClean", rsClean: 1, "2": "rsUpdate", rsUpdate: 2, "3": "rsDelete", rsDelete: 3};
  this.TDataRecord = function (s) {
    if (s) {
      this.data = s.data;
      this.state = s.state;
      this.bookmark = s.bookmark;
      this.bookmarkFlag = s.bookmarkFlag;
    } else {
      this.data = undefined;
      this.state = 0;
      this.bookmark = undefined;
      this.bookmarkFlag = 0;
    };
    this.$equal = function (b) {
      return (this.data === b.data) && ((this.state === b.state) && ((this.bookmark === b.bookmark) && (this.bookmarkFlag === b.bookmarkFlag)));
    };
  };
  rtl.createClass($mod,"TDataSet",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAfterLoad = null;
      this.FBeforeLoad = null;
      this.FBlockReadSize = 0;
      this.FCalcBuffer = new $mod.TDataRecord();
      this.FCalcFieldsCount = 0;
      this.FOnLoadFail = null;
      this.FOpenAfterRead = false;
      this.FActiveRecord = 0;
      this.FAfterCancel = null;
      this.FAfterClose = null;
      this.FAfterOpen = null;
      this.FAfterPost = null;
      this.FAfterScroll = null;
      this.FAutoCalcFields = false;
      this.FBOF = false;
      this.FBeforeCancel = null;
      this.FBeforeClose = null;
      this.FBeforeOpen = null;
      this.FBeforePost = null;
      this.FBeforeScroll = null;
      this.FBlobFieldCount = 0;
      this.FBuffers = [];
      this.FBufferCount = 0;
      this.FConstraints = null;
      this.FDisableControlsCount = 0;
      this.FDisableControlsState = 0;
      this.FCurrentRecord = 0;
      this.FDataSources = null;
      this.FDefaultFields = false;
      this.FEOF = false;
      this.FEnableControlsEvent = 0;
      this.FFieldList = null;
      this.FFieldDefs = null;
      this.FInternalCalcFields = false;
      this.FModified = false;
      this.FOnCalcFields = null;
      this.FOnPostError = null;
      this.FRecordCount = 0;
      this.FIsUniDirectional = false;
      this.FState = 0;
      this.FInternalOpenComplete = false;
      this.FDataProxy = null;
      this.FDataRequestID = 0;
      this.FChangeList = null;
    };
    this.$final = function () {
      this.FAfterLoad = undefined;
      this.FBeforeLoad = undefined;
      this.FCalcBuffer = undefined;
      this.FOnLoadFail = undefined;
      this.FAfterCancel = undefined;
      this.FAfterClose = undefined;
      this.FAfterOpen = undefined;
      this.FAfterPost = undefined;
      this.FAfterScroll = undefined;
      this.FBeforeCancel = undefined;
      this.FBeforeClose = undefined;
      this.FBeforeOpen = undefined;
      this.FBeforePost = undefined;
      this.FBeforeScroll = undefined;
      this.FBuffers = undefined;
      this.FConstraints = undefined;
      this.FDataSources = undefined;
      this.FFieldList = undefined;
      this.FFieldDefs = undefined;
      this.FOnCalcFields = undefined;
      this.FOnPostError = undefined;
      this.FDataProxy = undefined;
      this.FChangeList = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.DoInternalOpen = function () {
      this.InternalOpen();
      this.FInternalOpenComplete = true;
      this.FRecordCount = 0;
      this.RecalcBufListSize();
      this.FBOF = true;
      this.FEOF = this.FRecordCount === 0;
      if (this.GetDataProxy() != null) this.InitChangeList();
    };
    this.GetBufferCount = function () {
      var Result = 0;
      Result = rtl.length(this.FBuffers);
      return Result;
    };
    this.GetDataProxy = function () {
      var Result = null;
      if (this.FDataProxy === null) this.SetDataProxy(this.DoGetDataProxy());
      Result = this.FDataProxy;
      return Result;
    };
    this.RegisterDataSource = function (ADataSource) {
      this.FDataSources.Add(ADataSource);
      this.RecalcBufListSize();
    };
    this.SetDataProxy = function (AValue) {
      if (AValue === this.FDataProxy) return;
      if (this.FDataProxy != null) this.FDataProxy.RemoveFreeNotification(this);
      this.FDataProxy = AValue;
      if (this.FDataProxy != null) this.FDataProxy.FreeNotification(this);
    };
    this.ShiftBuffersForward = function () {
      var TempBuf = new $mod.TDataRecord();
      var I = 0;
      TempBuf = new $mod.TDataRecord(this.FBuffers[this.FBufferCount]);
      for (var $l1 = this.FBufferCount; $l1 >= 1; $l1--) {
        I = $l1;
        this.FBuffers[I] = new $mod.TDataRecord(this.FBuffers[I - 1]);
      };
      this.FBuffers[0] = new $mod.TDataRecord(TempBuf);
    };
    this.ShiftBuffersBackward = function () {
      var TempBuf = new $mod.TDataRecord();
      var I = 0;
      TempBuf = new $mod.TDataRecord(this.FBuffers[0]);
      for (var $l1 = 1, $end2 = this.FBufferCount; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FBuffers[I - 1] = new $mod.TDataRecord(this.FBuffers[I]);
      };
      this.FBuffers[this.GetBufferCount()] = new $mod.TDataRecord(TempBuf);
    };
    this.TryDoing = function (P, Ev) {
      var Result = false;
      var Retry = 0;
      Result = true;
      Retry = 2;
      while (Retry === 2) try {
        this.UpdateCursorPos();
        P();
        return Result;
      } catch ($e) {
        if ($mod.EDatabaseError.isPrototypeOf($e)) {
          var E = $e;
          Retry = 0;
          if (Ev != null) Ev(this,E,{get: function () {
              return Retry;
            }, set: function (v) {
              Retry = v;
            }});
          var $tmp1 = Retry;
          if ($tmp1 === 0) {
            throw $e}
           else if ($tmp1 === 1) pas.SysUtils.Abort();
        } else {
          throw $e;
        }
      };
      return Result;
    };
    this.GetActive = function () {
      var Result = false;
      Result = (this.FState !== 0) && (this.FState !== 12);
      return Result;
    };
    this.UnRegisterDataSource = function (ADataSource) {
      this.FDataSources.Remove(ADataSource);
    };
    this.HandleRequestresponse = function (ARequest) {
      var DataAdded = false;
      if (!(ARequest != null)) return;
      var $tmp1 = ARequest.FSuccess;
      if ($tmp1 === 0) {
        if (this.FOnLoadFail != null) this.FOnLoadFail(this,ARequest.FRequestID,ARequest.FErrorMsg);
      } else if (($tmp1 === 1) || ($tmp1 === 2)) {
        DataAdded = false;
        if (ARequest.FEvent != null) ARequest.FEvent(this,ARequest.FData);
        if (ARequest.FSuccess !== 1) DataAdded = this.DataPacketReceived(ARequest);
        if (!(this.GetActive() || (0 in ARequest.FLoadOptions))) {
          if (!(1 in ARequest.FLoadOptions)) this.DoAfterLoad();
          this.Open();
        } else {
          if ((2 in ARequest.FLoadOptions) && DataAdded) this.FEOF = false;
          if (!(1 in ARequest.FLoadOptions)) this.DoAfterLoad();
        };
      };
      ARequest.$destroy("Destroy");
    };
    this.DataPacketReceived = function (ARequest) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.DoLoad = function (aOptions, aAfterLoad) {
      var Result = false;
      var Request = null;
      if (!(1 in aOptions)) this.DoBeforeLoad();
      Result = this.GetDataProxy() !== null;
      if (!Result) return Result;
      Request = this.GetDataProxy().GetDataRequest(rtl.refSet(aOptions),rtl.createCallback(this,"HandleRequestresponse"),aAfterLoad);
      Request.FDataset = this;
      if (this.GetActive()) Request.FBookmark = new $mod.TBookmark(this.GetBookmark());
      this.FDataRequestID += 1;
      Request.FRequestID = this.FDataRequestID;
      this.GetDataProxy().DoGetData(Request);
      return Result;
    };
    this.DoGetDataProxy = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InitChangeList = function () {
      this.DoneChangeList();
      this.FChangeList = pas.Classes.TFPList.$create("Create");
    };
    this.DoneChangeList = function () {
      this.ClearChangeList();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FChangeList;
        }, set: function (v) {
          this.p.FChangeList = v;
        }});
    };
    this.ClearChangeList = function () {
      var I = 0;
      if (!(this.FChangeList != null)) return;
      for (var $l1 = 0, $end2 = this.FChangeList.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        rtl.getObject(this.FChangeList.Get(I)).$destroy("Destroy");
        this.FChangeList.Put(I,null);
      };
    };
    this.IndexInChangeList = function (aBookmark) {
      var Result = 0;
      Result = -1;
      if (!(this.FChangeList != null)) return Result;
      Result = this.FChangeList.FCount - 1;
      while ((Result >= 0) && (this.CompareBookmarks(new $mod.TBookmark(aBookmark),new $mod.TBookmark(rtl.getObject(this.FChangeList.Get(Result)).FBookmark)) !== 0)) Result -= 1;
      return Result;
    };
    this.AddToChangeList = function (aChange) {
      var Result = null;
      var B = new $mod.TBookmark();
      var I = 0;
      Result = null;
      if (!(this.FChangeList != null)) return Result;
      B = new $mod.TBookmark(this.GetBookmark());
      I = this.IndexInChangeList(new $mod.TBookmark(B));
      if (I === -1) {
        if (this.GetDataProxy() != null) {
          Result = this.GetDataProxy().GetUpdateDescriptor(this,new $mod.TBookmark(B),this.ActiveBuffer().data,aChange)}
         else Result = $mod.TRecordUpdateDescriptor.$create("Create$1",[null,this,new $mod.TBookmark(B),this.ActiveBuffer().data,aChange]);
        this.FChangeList.Add(Result);
      } else {
        Result = rtl.getObject(this.FChangeList.Get(I));
        var $tmp1 = aChange;
        if ($tmp1 === 3) {
          Result.FStatus = 3}
         else if ($tmp1 === 2) {
          $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SErrInsertingSameRecordtwice"),this)}
         else if ($tmp1 === 1) Result.FData = this.ActiveBuffer().data;
      };
      return Result;
    };
    this.RecalcBufListSize = function () {
      var i = 0;
      var j = 0;
      var ABufferCount = 0;
      var DataLink = null;
      if (!this.IsCursorOpen()) return;
      if (this.FIsUniDirectional) {
        ABufferCount = 1}
       else ABufferCount = 10;
      for (var $l1 = 0, $end2 = this.FDataSources.FCount - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        for (var $l3 = 0, $end4 = rtl.getObject(this.FDataSources.Get(i)).FDataLinks.GetCount() - 1; $l3 <= $end4; $l3++) {
          j = $l3;
          DataLink = rtl.getObject(rtl.getObject(this.FDataSources.Get(i)).FDataLinks.Get(j));
          if (ABufferCount < DataLink.GetBufferCount()) ABufferCount = DataLink.GetBufferCount();
        };
      };
      if (this.FBufferCount === ABufferCount) return;
      this.SetBufListSize(ABufferCount);
      this.GetNextRecords();
      if ((this.FRecordCount < this.FBufferCount) && !this.FIsUniDirectional) {
        this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
        this.CursorPosChanged();
      };
    };
    this.ActivateBuffers = function () {
      this.FBOF = false;
      this.FEOF = false;
      this.FActiveRecord = 0;
    };
    this.BindFields = function (Binding) {
      var i = 0;
      var FieldIndex = 0;
      var FieldDef = null;
      var Field = null;
      this.FCalcFieldsCount = 0;
      this.FBlobFieldCount = 0;
      for (var $l1 = 0, $end2 = this.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        Field = this.FFieldList.GetField(i);
        Field.FFieldDef = null;
        if (!Binding) {
          Field.FFieldNo = 0}
         else if (Field.FFieldKind in rtl.createSet(1,2)) {
          Field.FFieldNo = -1;
          this.FCalcFieldsCount += 1;
        } else {
          FieldIndex = this.FFieldDefs.IndexOf(Field.FFieldName);
          if (FieldIndex === -1) {
            $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[Field.FFieldName],this)}
           else {
            FieldDef = this.FFieldDefs.GetItem$1(FieldIndex);
            Field.FFieldDef = FieldDef;
            Field.FFieldNo = FieldDef.FFieldNo;
            if (FieldDef.FInternalCalcField) this.FInternalCalcFields = true;
            if (Field.$class.IsBlob()) {
              Field.FSize = FieldDef.FSize;
              this.FBlobFieldCount += 1;
            };
          };
        };
        Field.Bind(Binding);
      };
    };
    this.BlockReadNext = function () {
      this.MoveBy(1);
    };
    var BookmarkStates = rtl.createSet(1,2,3);
    this.BookmarkAvailable = function () {
      var Result = false;
      Result = ((!this.IsEmpty() && !this.FIsUniDirectional) && (this.FState in BookmarkStates)) && (this.GetBookmarkFlag(new $mod.TDataRecord(this.ActiveBuffer())) === 0);
      return Result;
    };
    this.CalculateFields = function (Buffer) {
      var i = 0;
      var OldState = 0;
      this.FCalcBuffer = new $mod.TDataRecord(Buffer.get());
      if (this.FState !== 11) {
        OldState = this.FState;
        this.FState = 5;
        try {
          this.ClearCalcFields({p: this, get: function () {
              return this.p.FCalcBuffer;
            }, set: function (v) {
              this.p.FCalcBuffer = v;
            }});
          if (!this.FIsUniDirectional) for (var $l1 = 0, $end2 = this.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
            i = $l1;
            if (this.FFieldList.GetField(i).FFieldKind === 2) this.FFieldList.GetField(i).CalcLookupValue();
          };
        } finally {
          this.DoOnCalcFields();
          this.FState = OldState;
        };
      };
    };
    this.CheckActive = function () {
      if (!this.GetActive()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SInactiveDataset"),this);
    };
    this.CheckInactive = function () {
      if (this.GetActive()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SActiveDataset"),this);
    };
    this.CheckBiDirectional = function () {
      if (this.FIsUniDirectional) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SUniDirectional"),this);
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.apply(this,arguments);
      try {
        if (this.FOpenAfterRead) this.SetActive(true);
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var E = $e;
          if (4 in this.FComponentState) this.InternalHandleException(E);
        } else {
          throw $e;
        }
      };
    };
    this.ClearBuffers = function () {
      this.FRecordCount = 0;
      this.FActiveRecord = 0;
      this.FCurrentRecord = -1;
      this.FBOF = true;
      this.FEOF = true;
    };
    this.ClearCalcFields = function (Buffer) {
    };
    this.CloseCursor = function () {
      this.ClearBuffers();
      this.SetBufListSize(0);
      this.FFieldList.ClearFieldDefs();
      this.InternalClose();
      this.FInternalOpenComplete = false;
    };
    this.CreateFields = function () {
      var I = 0;
      for (var $l1 = 0, $end2 = this.FFieldDefs.GetCount() - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        var $with3 = this.FFieldDefs.GetItem$1(I);
        if ($with3.FDataType !== 0) {
          $with3.CreateField(this);
        };
      };
    };
    this.DataEvent = function (Event, Info) {
      var Self = this;
      function HandleFieldChange(aField) {
        if (aField.FFieldKind in rtl.createSet(0,3)) Self.SetModified(true);
        if (Self.FState !== 4) {
          if (aField.FFieldKind === 0) {
            if (Self.FInternalCalcFields) {
              Self.RefreshInternalCalcFields({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
                  return this.p[this.a];
                }, set: function (v) {
                  this.p[this.a] = v;
                }})}
             else if (Self.FAutoCalcFields && (Self.FCalcFieldsCount !== 0)) Self.CalculateFields({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
                return this.p[this.a];
              }, set: function (v) {
                this.p[this.a] = v;
              }});
          };
          aField.Change();
        };
      };
      function HandleScrollOrChange() {
        if (Self.FState !== 3) Self.UpdateCursorPos();
      };
      var i = 0;
      var $tmp1 = Event;
      if ($tmp1 === 0) {
        HandleFieldChange(rtl.getObject(Info))}
       else if (($tmp1 === 2) || ($tmp1 === 3)) {
        HandleScrollOrChange()}
       else if ($tmp1 === 4) Self.FEnableControlsEvent = 4;
      if (!Self.ControlsDisabled() && (Self.FState !== 10)) {
        for (var $l2 = 0, $end3 = Self.FDataSources.FCount - 1; $l2 <= $end3; $l2++) {
          i = $l2;
          rtl.getObject(Self.FDataSources.Get(i)).ProcessEvent(Event,Info);
        };
      };
    };
    this.DestroyFields = function () {
      this.FFieldList.Clear();
    };
    this.DoAfterCancel = function () {
      if (this.FAfterCancel != null) this.FAfterCancel(this);
    };
    this.DoAfterClose = function () {
      if ((this.FAfterClose != null) && !(3 in this.FComponentState)) this.FAfterClose(this);
    };
    this.DoAfterOpen = function () {
      if (this.FAfterOpen != null) this.FAfterOpen(this);
    };
    this.DoAfterPost = function () {
      if (this.FAfterPost != null) this.FAfterPost(this);
    };
    this.DoAfterScroll = function () {
      if (this.FAfterScroll != null) this.FAfterScroll(this);
    };
    this.DoBeforeCancel = function () {
      if (this.FBeforeCancel != null) this.FBeforeCancel(this);
    };
    this.DoBeforeClose = function () {
      if ((this.FBeforeClose != null) && !(3 in this.FComponentState)) this.FBeforeClose(this);
    };
    this.DoBeforeOpen = function () {
      if (this.FBeforeOpen != null) this.FBeforeOpen(this);
    };
    this.DoBeforePost = function () {
      if (this.FBeforePost != null) this.FBeforePost(this);
    };
    this.DoBeforeScroll = function () {
      if (this.FBeforeScroll != null) this.FBeforeScroll(this);
    };
    this.DoOnCalcFields = function () {
      if (this.FOnCalcFields != null) this.FOnCalcFields(this);
    };
    this.DoBeforeLoad = function () {
      if (this.FBeforeLoad != null) this.FBeforeLoad(this);
    };
    this.DoAfterLoad = function () {
      if (this.FAfterLoad != null) this.FAfterLoad(this);
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      Result = $mod.DefaultFieldClasses[FieldType];
      return Result;
    };
    this.GetfieldCount = function () {
      var Result = 0;
      Result = this.FFieldList.GetCount();
      return Result;
    };
    this.GetNextRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetNextRecord()) Result += 1;
      return Result;
    };
    this.GetNextRecord = function () {
      var Result = false;
      var T = new $mod.TDataRecord();
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FRecordCount - 1);
      Result = this.GetRecord({a: this.FBufferCount, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},1,true) === 0;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        if (this.FRecordCount === this.FBufferCount) {
          this.ShiftBuffersBackward()}
         else {
          this.FRecordCount += 1;
          this.FCurrentRecord = this.FRecordCount - 1;
          T = new $mod.TDataRecord(this.FBuffers[this.FCurrentRecord]);
          this.FBuffers[this.FCurrentRecord] = new $mod.TDataRecord(this.FBuffers[this.FBufferCount]);
          this.FBuffers[this.FBufferCount] = new $mod.TDataRecord(T);
        };
      } else this.CursorPosChanged();
      return Result;
    };
    this.GetPriorRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetPriorRecord()) Result += 1;
      return Result;
    };
    this.GetPriorRecord = function () {
      var Result = false;
      this.CheckBiDirectional();
      if (this.FRecordCount > 0) this.SetCurrentRecord(0);
      Result = this.GetRecord({a: this.FBufferCount, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},2,true) === 0;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        this.ShiftBuffersForward();
        if (this.FRecordCount < this.FBufferCount) this.FRecordCount += 1;
      } else this.CursorPosChanged();
      return Result;
    };
    this.InitRecord = function (Buffer) {
      this.InternalInitRecord(Buffer);
      this.ClearCalcFields(Buffer);
    };
    this.InternalCancel = function () {
    };
    this.OpenCursor = function (InfoQuery) {
      if (InfoQuery) {
        this.InternalInitFieldDefs()}
       else if (this.FState !== 12) this.DoInternalOpen();
    };
    this.OpenCursorcomplete = function () {
      try {
        if (this.FState === 12) this.DoInternalOpen();
      } finally {
        if (this.FInternalOpenComplete) {
          this.SetState(1);
          this.DoAfterOpen();
          if (!this.IsEmpty()) this.DoAfterScroll();
        } else {
          this.SetState(0);
          this.CloseCursor();
        };
      };
    };
    this.RefreshInternalCalcFields = function (Buffer) {
    };
    this.SetActive = function (Value) {
      if (Value && (this.FState === 0)) {
        if (0 in this.FComponentState) {
          this.FOpenAfterRead = true;
          return;
        } else {
          this.DoBeforeOpen();
          this.FEnableControlsEvent = 4;
          this.FInternalCalcFields = false;
          try {
            this.FDefaultFields = this.GetfieldCount() === 0;
            this.OpenCursor(false);
          } finally {
            if (this.FState !== 12) this.OpenCursorcomplete();
          };
        };
        this.FModified = false;
      } else if (!Value && (this.FState !== 0)) {
        this.DoBeforeClose();
        this.SetState(0);
        this.FDataRequestID = 0;
        this.DoneChangeList();
        this.CloseCursor();
        this.DoAfterClose();
        this.FModified = false;
      };
    };
    this.SetBufListSize = function (Value) {
      var I = 0;
      if (Value < 0) Value = 0;
      if (Value === this.FBufferCount) return;
      if (Value > this.GetBufferCount()) {
        for (var $l1 = this.FBufferCount, $end2 = Value; $l1 <= $end2; $l1++) {
          I = $l1;
          this.FBuffers[I] = new $mod.TDataRecord(this.AllocRecordBuffer());
        };
      } else if (Value < this.GetBufferCount()) if ((Value >= 0) && (this.FActiveRecord > (Value - 1))) {
        for (var $l3 = 0, $end4 = this.FActiveRecord - Value; $l3 <= $end4; $l3++) {
          I = $l3;
          this.ShiftBuffersBackward();
        };
        this.FActiveRecord = Value - 1;
      };
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,Value + 1);
      this.FBufferCount = Value;
      if (this.FRecordCount > this.FBufferCount) this.FRecordCount = this.FBufferCount;
    };
    this.SetCurrentRecord = function (Index) {
      if (this.FCurrentRecord !== Index) {
        if (!this.FIsUniDirectional) {
          var $tmp1 = this.GetBookmarkFlag(new $mod.TDataRecord(this.FBuffers[Index]));
          if ($tmp1 === 0) {
            this.InternalSetToRecord(new $mod.TDataRecord(this.FBuffers[Index]))}
           else if ($tmp1 === 1) {
            this.InternalFirst()}
           else if ($tmp1 === 2) this.InternalLast();
        };
        this.FCurrentRecord = Index;
      };
    };
    this.SetModified = function (Value) {
      this.FModified = Value;
    };
    this.SetName = function (NewName) {
      var Self = this;
      function CheckName(FieldName) {
        var Result = "";
        var i = 0;
        var j = 0;
        Result = FieldName;
        i = 0;
        j = 0;
        while (i < Self.FFieldList.GetCount()) {
          if (Result === Self.FFieldList.GetField(i).FFieldName) {
            j += 1;
            Result = FieldName + pas.SysUtils.IntToStr(j);
          } else i += 1;
        };
        return Result;
      };
      var i = 0;
      var nm = "";
      var old = "";
      if (Self.FName === NewName) return;
      old = Self.FName;
      pas.Classes.TComponent.SetName.call(Self,NewName);
      if (4 in Self.FComponentState) for (var $l1 = 0, $end2 = Self.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        nm = old + Self.FFieldList.GetField(i).FFieldName;
        if (pas.System.Copy(Self.FFieldList.GetField(i).FName,1,nm.length) === nm) Self.FFieldList.GetField(i).SetName(CheckName(NewName + Self.FFieldList.GetField(i).FFieldName));
      };
    };
    this.SetState = function (Value) {
      if (Value !== this.FState) {
        this.FState = Value;
        if (Value === 1) this.FModified = false;
        this.DataEvent(6,0);
      };
    };
    this.AllocRecordBuffer = function () {
      var Result = new $mod.TDataRecord();
      Result.data = null;
      Result.state = 0;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
    };
    this.GetBookmarkData = function (Buffer, Data) {
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.InternalFirst = function () {
    };
    this.InternalHandleException = function (E) {
      pas.SysUtils.ShowException(E,null);
    };
    this.InternalInitRecord = function (Buffer) {
    };
    this.InternalLast = function () {
    };
    this.InternalPost = function () {
      var Self = this;
      function CheckRequiredFields() {
        var I = 0;
        for (var $l1 = 0, $end2 = Self.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          var $with3 = Self.FFieldList.GetField(I);
          if (((($with3.FRequired && !$with3.FReadOnly) && ($with3.FFieldKind === 0)) && !($with3.FDataType === 9)) && $with3.GetIsNull()) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNeedField"),[$with3.GetDisplayName()],Self);
        };
      };
      CheckRequiredFields();
    };
    this.InternalSetToRecord = function (Buffer) {
    };
    this.Notification = function (AComponent, Operation) {
      pas.Classes.TComponent.Notification.call(this,AComponent,Operation);
      if ((Operation === 1) && (AComponent === this.FDataProxy)) this.FDataProxy = null;
    };
    this.GetFieldData = function (Field) {
      var Result = undefined;
      Result = this.GetFieldData$1(Field,new $mod.TDataRecord(this.ActiveBuffer()));
      return Result;
    };
    this.SetFieldData = function (Field, AValue) {
      this.SetFieldData$1(Field,{a: this.FActiveRecord, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},AValue);
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      Result = rtl.getObject(Buffer.data)[Field.FFieldName];
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      rtl.getObject(Buffer.get().data)[Field.FFieldName] = AValue;
    };
    this.FieldDefsClass = function () {
      var Result = null;
      Result = $mod.TFieldDefs;
      return Result;
    };
    this.FieldsClass = function () {
      var Result = null;
      Result = $mod.TFields;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FFieldDefs = this.$class.FieldDefsClass().$create("Create$4",[this]);
      this.FFieldList = this.$class.FieldsClass().$create("Create$1",[this]);
      this.FDataSources = pas.Classes.TFPList.$create("Create");
      this.FConstraints = $mod.TCheckConstraints.$create("Create$2",[this]);
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,1);
      this.FActiveRecord = 0;
      this.FEOF = true;
      this.FBOF = true;
      this.FIsUniDirectional = false;
      this.FAutoCalcFields = true;
      this.FDataRequestID = 0;
    };
    this.Destroy = function () {
      var i = 0;
      this.SetActive(false);
      rtl.free(this,"FFieldDefs");
      rtl.free(this,"FFieldList");
      var $with1 = this.FDataSources;
      while ($with1.FCount > 0) rtl.getObject($with1.Get($with1.FCount - 1)).SetDataSet(null);
      $with1.$destroy("Destroy");
      for (var $l2 = 0, $end3 = this.FBufferCount; $l2 <= $end3; $l2++) {
        i = $l2;
        this.FreeRecordBuffer({a: i, p: this.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
      };
      rtl.free(this,"FConstraints");
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,1);
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.ActiveBuffer = function () {
      var Result = new $mod.TDataRecord();
      Result = new $mod.TDataRecord(this.FBuffers[this.FActiveRecord]);
      return Result;
    };
    this.ConvertToDateTime = function (aValue, ARaiseException) {
      var Result = 0.0;
      Result = this.$class.DefaultConvertToDateTime(aValue,ARaiseException);
      return Result;
    };
    this.ConvertDateTimeToNative = function (aValue) {
      var Result = undefined;
      Result = this.$class.DefaultConvertDateTimeToNative(aValue);
      return Result;
    };
    this.DefaultConvertToDateTime = function (aValue, ARaiseException) {
      var Result = 0.0;
      Result = 0;
      if (rtl.isString(aValue)) {
        if (!pas.DateUtils.TryRFC3339ToDateTime("" + aValue,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})) throw pas.SysUtils.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.DBConst,"SErrInvalidDateTime"),["" + aValue]]);
      } else if (rtl.isNumber(aValue)) Result = rtl.getNumber(aValue);
      return Result;
    };
    this.DefaultConvertDateTimeToNative = function (aValue) {
      var Result = undefined;
      Result = pas.DateUtils.DateTimeToRFC3339(aValue);
      return Result;
    };
    this.BlobDataToBytes = function (aValue) {
      var Result = [];
      Result = this.$class.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.DefaultBlobDataToBytes = function (aValue) {
      var Result = [];
      var S = "";
      var I = 0;
      var J = 0;
      var L = 0;
      Result = rtl.arraySetLength(Result,0,0);
      if (rtl.isString(aValue)) {
        S = "" + aValue;
        L = S.length;
        Result = rtl.arraySetLength(Result,0,Math.floor((L + 1) / 2));
        I = 1;
        J = 0;
        while (I < L) {
          Result[J] = pas.SysUtils.StrToInt("$" + pas.System.Copy(S,I,2));
          I += 2;
          J += 1;
        };
      };
      return Result;
    };
    this.Cancel = function () {
      if (this.FState in rtl.createSet(2,3)) {
        this.DataEvent(7,0);
        this.DoBeforeCancel();
        this.UpdateCursorPos();
        this.InternalCancel();
        if ((this.FState === 3) && (this.FRecordCount === 1)) {
          this.FEOF = true;
          this.FBOF = true;
          this.FRecordCount = 0;
          this.InitRecord({a: this.FActiveRecord, p: this.FBuffers, get: function () {
              return this.p[this.a];
            }, set: function (v) {
              this.p[this.a] = v;
            }});
          this.SetState(1);
          this.DataEvent(2,0);
        } else {
          this.SetState(1);
          this.SetCurrentRecord(this.FActiveRecord);
          this.Resync({});
        };
        this.DoAfterCancel();
      };
    };
    this.CheckBrowseMode = function () {
      this.CheckActive();
      this.DataEvent(7,0);
      var $tmp1 = this.FState;
      if (($tmp1 === 2) || ($tmp1 === 3)) {
        this.UpdateRecord();
        if (this.FModified) {
          this.Post()}
         else this.Cancel();
      } else if ($tmp1 === 4) this.Post();
    };
    this.ControlsDisabled = function () {
      var Result = false;
      Result = this.FDisableControlsCount > 0;
      return Result;
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.CursorPosChanged = function () {
      this.FCurrentRecord = -1;
    };
    this.DisableControls = function () {
      if (this.FDisableControlsCount === 0) {
        this.FDisableControlsState = this.FState;
        this.FEnableControlsEvent = 2;
      };
      this.FDisableControlsCount += 1;
    };
    this.EnableControls = function () {
      if (this.FDisableControlsCount > 0) this.FDisableControlsCount -= 1;
      if (this.FDisableControlsCount === 0) {
        if (this.FState !== this.FDisableControlsState) this.DataEvent(6,0);
        if ((this.FState !== 0) && (this.FDisableControlsState !== 0)) this.DataEvent(this.FEnableControlsEvent,0);
      };
    };
    this.FieldByName = function (FieldName) {
      var Result = null;
      Result = this.FindField(FieldName);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[FieldName],this);
      return Result;
    };
    this.FindField = function (FieldName) {
      var Result = null;
      Result = this.FFieldList.FindField(FieldName);
      return Result;
    };
    this.First = function () {
      this.CheckBrowseMode();
      this.DoBeforeScroll();
      if (!this.FIsUniDirectional) {
        this.ClearBuffers()}
       else if (!this.FBOF) {
        this.SetActive(false);
        this.SetActive(true);
      };
      try {
        this.InternalFirst();
        if (!this.FIsUniDirectional) this.GetNextRecords();
      } finally {
        this.FBOF = true;
        this.DataEvent(2,0);
        this.DoAfterScroll();
      };
    };
    this.GetBookmark = function () {
      var Result = new $mod.TBookmark();
      if (this.BookmarkAvailable()) {
        this.GetBookmarkData(new $mod.TDataRecord(this.ActiveBuffer()),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})}
       else Result.Data = null;
      return Result;
    };
    this.IsEmpty = function () {
      var Result = false;
      Result = (this.FBOF && this.FEOF) && !(this.FState === 3);
      return Result;
    };
    this.MoveBy = function (Distance) {
      var Self = this;
      var Result = 0;
      var TheResult = 0;
      function ScrollForward() {
        var Result = 0;
        Result = 0;
        Self.FBOF = false;
        while ((Distance > 0) && !Self.FEOF) {
          if (Self.FActiveRecord < (Self.FRecordCount - 1)) {
            Self.FActiveRecord += 1;
            Distance -= 1;
            TheResult += 1;
          } else {
            if (Self.GetNextRecord()) {
              Distance -= 1;
              Result -= 1;
              TheResult += 1;
            } else {
              Self.FEOF = true;
              Self.DoLoad(rtl.createSet(0,2),null);
            };
          };
        };
        return Result;
      };
      function ScrollBackward() {
        var Result = 0;
        Self.CheckBiDirectional();
        Result = 0;
        Self.FEOF = false;
        while ((Distance < 0) && !Self.FBOF) {
          if (Self.FActiveRecord > 0) {
            Self.FActiveRecord -= 1;
            Distance += 1;
            TheResult -= 1;
          } else {
            if (Self.GetPriorRecord()) {
              Distance += 1;
              Result += 1;
              TheResult -= 1;
            } else Self.FBOF = true;
          };
        };
        return Result;
      };
      var Scrolled = 0;
      Self.CheckBrowseMode();
      Result = 0;
      TheResult = 0;
      Self.DoBeforeScroll();
      if (((Distance === 0) || ((Distance > 0) && Self.FEOF)) || ((Distance < 0) && Self.FBOF)) return Result;
      try {
        Scrolled = 0;
        if (Distance > 0) {
          Scrolled = ScrollForward()}
         else Scrolled = ScrollBackward();
      } finally {
        Self.DataEvent(3,Scrolled);
        Self.DoAfterScroll();
        Result = TheResult;
      };
      return Result;
    };
    this.Next = function () {
      if (this.FBlockReadSize > 0) {
        this.BlockReadNext()}
       else this.MoveBy(1);
    };
    this.Open = function () {
      this.SetActive(true);
    };
    var UpdateStates = [1,2];
    this.Post = function () {
      var R = null;
      var WasInsert = false;
      this.UpdateRecord();
      if (this.FState in rtl.createSet(2,3)) {
        this.DataEvent(7,0);
        this.DoBeforePost();
        WasInsert = this.FState === 3;
        if (!this.TryDoing(rtl.createCallback(this,"InternalPost"),this.FOnPostError)) return;
        this.CursorPosChanged();
        this.SetState(1);
        this.Resync({});
        R = this.AddToChangeList(UpdateStates[+WasInsert]);
        if (R != null) R.FBookmark = new $mod.TBookmark(this.GetBookmark());
        this.DoAfterPost();
      } else if (this.FState !== 4) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNotEditing"),[this.FName],this);
    };
    this.Resync = function (Mode) {
      var i = 0;
      var count = 0;
      if (this.FIsUniDirectional) return;
      if (this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},0,false) !== 0) if (0 in Mode) {
        $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SNoSuchRecord"),this)}
       else if ((this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},1,true) !== 0) && (this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},2,true) !== 0)) {
        this.ClearBuffers();
        this.InternalInitRecord({a: this.FActiveRecord, p: this.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
        this.DataEvent(2,0);
        return;
      };
      this.FCurrentRecord = 0;
      this.FEOF = false;
      this.FBOF = false;
      if (1 in Mode) {
        count = Math.floor(this.FRecordCount / 2)}
       else count = this.FActiveRecord;
      i = 0;
      this.FRecordCount = 1;
      this.FActiveRecord = 0;
      while ((i < count) && this.GetPriorRecord()) i += 1;
      this.FActiveRecord = i;
      this.GetNextRecords();
      if (this.FRecordCount < this.FBufferCount) this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
      this.DataEvent(2,0);
    };
    this.UpdateCursorPos = function () {
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FActiveRecord);
    };
    this.UpdateRecord = function () {
      if (!(this.FState in $mod.dsEditModes)) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNotEditing"),[this.FName],this);
      this.DataEvent(5,0);
    };
  });
  rtl.createClass($mod,"TDataLink",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FFirstRecord = 0;
      this.FBufferCount = 0;
      this.FActive = false;
      this.FDataSourceFixed = false;
      this.FEditing = false;
      this.FReadOnly = false;
      this.FUpdatingRecord = false;
      this.FVisualControl = false;
      this.FDataSource = null;
    };
    this.$final = function () {
      this.FDataSource = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.CalcFirstRecord = function (Index) {
      var Result = 0;
      if (this.FDataSource.FDataSet.FActiveRecord > (((this.FFirstRecord + Index) + this.FBufferCount) - 1)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - (((this.FFirstRecord + Index) + this.FBufferCount) - 1)}
       else if (this.FDataSource.FDataSet.FActiveRecord < (this.FFirstRecord + Index)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - (this.FFirstRecord + Index)}
       else Result = 0;
      this.FFirstRecord += Index + Result;
      return Result;
    };
    this.CalcRange = function () {
      var aMax = 0;
      var aMin = 0;
      aMin = (this.GetDataset().FActiveRecord - this.FBufferCount) + 1;
      if (aMin < 0) aMin = 0;
      aMax = this.GetDataset().FBufferCount - this.FBufferCount;
      if (aMax < 0) aMax = 0;
      if (aMax > this.GetDataset().FActiveRecord) aMax = this.GetDataset().FActiveRecord;
      if (this.FFirstRecord < aMin) this.FFirstRecord = aMin;
      if (this.FFirstRecord > aMax) this.FFirstRecord = aMax;
      if ((this.FFirstRecord !== 0) && ((this.GetDataset().FActiveRecord - this.FFirstRecord) < (this.FBufferCount - 1))) this.FFirstRecord -= 1;
    };
    this.CheckActiveAndEditing = function () {
      var B = false;
      B = (this.FDataSource != null) && !(this.FDataSource.FState in rtl.createSet(0,12));
      if (B !== this.FActive) {
        this.FActive = B;
        this.ActiveChanged();
      };
      B = ((this.FDataSource != null) && (this.FDataSource.FState in $mod.dsEditModes)) && !this.FReadOnly;
      if (B !== this.FEditing) {
        this.FEditing = B;
        this.EditingChanged();
      };
    };
    this.GetDataset = function () {
      var Result = null;
      if (this.FDataSource != null) {
        Result = this.FDataSource.FDataSet}
       else Result = null;
      return Result;
    };
    this.SetActive = function (AActive) {
      if (this.FActive !== AActive) {
        this.FActive = AActive;
        this.ActiveChanged();
      };
    };
    this.SetDataSource = function (Value) {
      if (this.FDataSource === Value) return;
      if (!this.FDataSourceFixed) {
        if (this.FDataSource != null) {
          this.FDataSource.UnregisterDataLink(this);
          this.FDataSource = null;
          this.CheckActiveAndEditing();
        };
        this.FDataSource = Value;
        if (this.FDataSource != null) {
          this.FDataSource.RegisterDataLink(this);
          this.CheckActiveAndEditing();
        };
      };
    };
    this.ActiveChanged = function () {
      this.FFirstRecord = 0;
    };
    this.CheckBrowseMode = function () {
    };
    this.DataEvent = function (Event, Info) {
      var $tmp1 = Event;
      if (($tmp1 === 0) || ($tmp1 === 1)) {
        if (!this.FUpdatingRecord) this.RecordChanged(rtl.getObject(Info))}
       else if ($tmp1 === 2) {
        this.SetActive(this.FDataSource.FDataSet.GetActive());
        this.CalcRange();
        this.CalcFirstRecord(Math.floor(Info));
        this.DataSetChanged();
      } else if ($tmp1 === 3) {
        this.DataSetScrolled(this.CalcFirstRecord(Math.floor(Info)))}
       else if ($tmp1 === 4) {
        this.CalcFirstRecord(Math.floor(Info));
        this.LayoutChanged();
      } else if ($tmp1 === 5) {
        this.UpdateRecord()}
       else if ($tmp1 === 6) {
        this.CheckActiveAndEditing()}
       else if ($tmp1 === 7) {
        this.CheckBrowseMode()}
       else if ($tmp1 === 10) this.FocusControl(Info);
    };
    this.DataSetChanged = function () {
      this.RecordChanged(null);
    };
    this.DataSetScrolled = function (Distance) {
      this.DataSetChanged();
    };
    this.EditingChanged = function () {
    };
    this.FocusControl = function (Field) {
    };
    this.GetBufferCount = function () {
      var Result = 0;
      Result = this.FBufferCount;
      return Result;
    };
    this.LayoutChanged = function () {
      this.DataSetChanged();
    };
    this.RecordChanged = function (Field) {
    };
    this.UpdateData = function () {
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FBufferCount = 1;
      this.FFirstRecord = 0;
      this.FDataSource = null;
      this.FDataSourceFixed = false;
    };
    this.Destroy = function () {
      this.FActive = false;
      this.FEditing = false;
      this.FDataSourceFixed = false;
      this.SetDataSource(null);
      pas.System.TObject.Destroy.call(this);
    };
    this.UpdateRecord = function () {
      this.FUpdatingRecord = true;
      try {
        this.UpdateData();
      } finally {
        this.FUpdatingRecord = false;
      };
    };
  });
  $mod.$rtti.$MethodVar("TDataChangeEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Field",$mod.$rtti["TField"]]]), methodkind: 0});
  rtl.createClass($mod,"TDataSource",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FDataSet = null;
      this.FDataLinks = null;
      this.FEnabled = false;
      this.FAutoEdit = false;
      this.FState = 0;
      this.FOnStateChange = null;
      this.FOnDataChange = null;
      this.FOnUpdateData = null;
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FDataLinks = undefined;
      this.FOnStateChange = undefined;
      this.FOnDataChange = undefined;
      this.FOnUpdateData = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.DistributeEvent = function (Event, Info) {
      var i = 0;
      var $with1 = this.FDataLinks;
      for (var $l2 = 0, $end3 = $with1.GetCount() - 1; $l2 <= $end3; $l2++) {
        i = $l2;
        var $with4 = rtl.getObject($with1.Get(i));
        if (!$with4.FVisualControl) $with4.DataEvent(Event,Info);
      };
      for (var $l5 = 0, $end6 = $with1.GetCount() - 1; $l5 <= $end6; $l5++) {
        i = $l5;
        var $with7 = rtl.getObject($with1.Get(i));
        if ($with7.FVisualControl) $with7.DataEvent(Event,Info);
      };
    };
    this.RegisterDataLink = function (DataLink) {
      this.FDataLinks.Add(DataLink);
      if (this.FDataSet != null) this.FDataSet.RecalcBufListSize();
    };
    var OnDataChangeEvents = rtl.createSet(1,2,3,4,6);
    this.ProcessEvent = function (Event, Info) {
      var NeedDataChange = false;
      var FLastState = 0;
      if (Event === 6) {
        NeedDataChange = this.FState === 0;
        FLastState = this.FState;
        if (this.FDataSet != null) {
          this.FState = this.FDataSet.FState}
         else this.FState = 0;
        if (this.FState === FLastState) return;
      } else NeedDataChange = true;
      this.DistributeEvent(Event,Info);
      if (!(3 in this.FComponentState)) {
        if (Event === 6) this.DoStateChange();
        if ((Event in OnDataChangeEvents) && NeedDataChange) this.DoDataChange(null);
        if (Event === 0) this.DoDataChange(Info);
        if (Event === 5) this.DoUpdateData();
      };
    };
    this.SetDataSet = function (ADataSet) {
      if (this.FDataSet !== null) {
        this.FDataSet.UnRegisterDataSource(this);
        this.FDataSet = null;
        this.ProcessEvent(6,0);
      };
      if (ADataSet !== null) {
        ADataSet.RegisterDataSource(this);
        this.FDataSet = ADataSet;
        this.ProcessEvent(6,0);
      };
    };
    this.SetEnabled = function (Value) {
      this.FEnabled = Value;
    };
    this.UnregisterDataLink = function (DataLink) {
      this.FDataLinks.Remove(DataLink);
      if (this.FDataSet !== null) this.FDataSet.RecalcBufListSize();
    };
    this.DoDataChange = function (Info) {
      if (this.FOnDataChange != null) this.FOnDataChange(this,Info);
    };
    this.DoStateChange = function () {
      if (this.FOnStateChange != null) this.FOnStateChange(this);
    };
    this.DoUpdateData = function () {
      if (this.FOnUpdateData != null) this.FOnUpdateData(this);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FDataLinks = pas.Classes.TList.$create("Create$1");
      this.FEnabled = true;
      this.FAutoEdit = true;
    };
    this.Destroy = function () {
      this.FOnStateChange = null;
      this.SetDataSet(null);
      var $with1 = this.FDataLinks;
      while ($with1.GetCount() > 0) rtl.getObject($with1.Get($with1.GetCount() - 1)).SetDataSource(null);
      rtl.free(this,"FDataLinks");
      pas.Classes.TComponent.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("AutoEdit",0,rtl.boolean,"FAutoEdit","FAutoEdit",{Default: true});
    $r.addProperty("DataSet",2,$mod.$rtti["TDataSet"],"FDataSet","SetDataSet");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("OnStateChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStateChange","FOnStateChange");
    $r.addProperty("OnDataChange",0,$mod.$rtti["TDataChangeEvent"],"FOnDataChange","FOnDataChange");
    $r.addProperty("OnUpdateData",0,pas.Classes.$rtti["TNotifyEvent"],"FOnUpdateData","FOnUpdateData");
  });
  this.TDataRequestResult = {"0": "rrFail", rrFail: 0, "1": "rrEOF", rrEOF: 1, "2": "rrOK", rrOK: 2};
  rtl.createClass($mod,"TDataRequest",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FBookmark = new $mod.TBookmark();
      this.FDataset = null;
      this.FErrorMsg = "";
      this.FEvent = null;
      this.FLoadOptions = {};
      this.FRequestID = 0;
      this.FSuccess = 0;
      this.FData = undefined;
      this.FAfterRequest = null;
      this.FDataProxy = null;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FEvent = undefined;
      this.FLoadOptions = undefined;
      this.FAfterRequest = undefined;
      this.FDataProxy = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (aDataProxy, aOptions, aAfterRequest, aAfterLoad) {
      this.FDataProxy = aDataProxy;
      this.FLoadOptions = rtl.refSet(aOptions);
      this.FEvent = aAfterLoad;
      this.FAfterRequest = aAfterRequest;
    };
  });
  rtl.createClass($mod,"TRecordUpdateDescriptor",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FBookmark = new $mod.TBookmark();
      this.FData = undefined;
      this.FDataset = null;
      this.FProxy = null;
      this.FStatus = 0;
      this.FOriginalStatus = 0;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FProxy = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (aProxy, aDataset, aBookmark, AData, AStatus) {
      this.FDataset = aDataset;
      this.FBookmark = new $mod.TBookmark(aBookmark);
      this.FData = AData;
      this.FStatus = AStatus;
      this.FOriginalStatus = AStatus;
      this.FProxy = aProxy;
    };
  });
  rtl.createClass($mod,"TDataProxy",pas.Classes.TComponent,function () {
    this.GetDataRequestClass = function () {
      var Result = null;
      Result = $mod.TDataRequest;
      return Result;
    };
    this.GetUpdateDescriptorClass = function () {
      var Result = null;
      Result = $mod.TRecordUpdateDescriptor;
      return Result;
    };
    this.GetDataRequest = function (aOptions, aAfterRequest, aAfterLoad) {
      var Result = null;
      Result = this.GetDataRequestClass().$create("Create$1",[this,rtl.refSet(aOptions),aAfterRequest,aAfterLoad]);
      return Result;
    };
    this.GetUpdateDescriptor = function (aDataset, aBookmark, AData, AStatus) {
      var Result = null;
      Result = this.GetUpdateDescriptorClass().$create("Create$1",[this,aDataset,new $mod.TBookmark(aBookmark),AData,AStatus]);
      return Result;
    };
  });
  this.DefaultFieldClasses = [$mod.TField,$mod.TStringField,$mod.TIntegerField,$mod.TLargeintField,$mod.TBooleanField,$mod.TFloatField,$mod.TDateField,$mod.TTimeField,$mod.TDateTimeField,$mod.TAutoIncField,$mod.TBlobField,$mod.TMemoField,$mod.TStringField,$mod.TVariantField,null];
  this.dsEditModes = rtl.createSet(2,3,4);
  this.ftBlobTypes = rtl.createSet(10,11);
  this.DatabaseError$1 = function (Msg, Comp) {
    if ((Comp != null) && (Comp.FName !== "")) throw $mod.EDatabaseError.$create("CreateFmt",["%s : %s",[Comp.FName,Msg]]);
  };
  this.DatabaseErrorFmt = function (Fmt, Args) {
    throw $mod.EDatabaseError.$create("CreateFmt",[Fmt,Args]);
  };
  this.DatabaseErrorFmt$1 = function (Fmt, Args, Comp) {
    if (Comp != null) throw $mod.EDatabaseError.$create("CreateFmt",[pas.SysUtils.Format("%s : %s",[Comp.FName,Fmt]),Args]);
  };
  this.ExtractFieldName = function (Fields, Pos) {
    var Result = "";
    var i = 0;
    var FieldsLength = 0;
    i = Pos.get();
    FieldsLength = Fields.length;
    while ((i <= FieldsLength) && (Fields.charAt(i - 1) !== ";")) i += 1;
    Result = pas.SysUtils.Trim(pas.System.Copy(Fields,Pos.get(),i - Pos.get()));
    if ((i <= FieldsLength) && (Fields.charAt(i - 1) === ";")) i += 1;
    Pos.set(i);
    return Result;
  };
  $mod.$init = function () {
  };
},["DBConst"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.DefaultBufferCount = 10;
  $impl.SInteger = "Integer";
  $impl.SLargeInt = "NativeInt";
  $impl.SJSValue = "JSValue";
  $impl.SBytes = "Bytes";
});
rtl.module("JSONDataset",["System","Types","JS","DB","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TJSONFieldMapper",pas.System.TObject,function () {
    this.GetJSONDataForField$1 = function (F, Row) {
      var Result = undefined;
      Result = this.GetJSONDataForField(F.FFieldName,F.GetIndex(),Row);
      return Result;
    };
    this.SetJSONDataForField$1 = function (F, Row, Data) {
      this.SetJSONDataForField(F.FFieldName,F.GetIndex(),Row,Data);
    };
  });
  rtl.createClass($mod,"TJSONDateField",pas.DB.TDateField,function () {
    this.$init = function () {
      pas.DB.TDateField.$init.call(this);
      this.FDateFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateFormat",0,rtl.string,"FDateFormat","FDateFormat");
  });
  rtl.createClass($mod,"TJSONTimeField",pas.DB.TTimeField,function () {
    this.$init = function () {
      pas.DB.TTimeField.$init.call(this);
      this.FTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("TimeFormat",0,rtl.string,"FTimeFormat","FTimeFormat");
  });
  rtl.createClass($mod,"TJSONDateTimeField",pas.DB.TDateTimeField,function () {
    this.$init = function () {
      pas.DB.TDateTimeField.$init.call(this);
      this.FDateTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateTimeFormat",0,rtl.string,"FDateTimeFormat","FDateTimeFormat");
  });
  rtl.createClass($mod,"TJSONIndex",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
      this.FRows = null;
      this.FDataset = null;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FRows = undefined;
      this.FDataset = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.GetRecordIndex = function (aListIndex) {
      var Result = 0;
      if (pas.JS.isUndefined(this.FList[aListIndex])) {
        Result = -1}
       else Result = Math.floor(this.FList[aListIndex]);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.length;
      return Result;
    };
    this.Create$1 = function (aDataset, aRows) {
      this.FRows = aRows;
      this.FList = new Array(this.FRows.length);
      this.FDataset = aDataset;
      this.CreateIndex();
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      Result = this.Append(aRecordIndex);
      return Result;
    };
  });
  rtl.createClass($mod,"TDefaultJSONIndex",$mod.TJSONIndex,function () {
    this.CreateIndex = function () {
      var I = 0;
      for (var $l1 = 0, $end2 = this.FRows.length - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FList[I] = I;
      };
    };
    this.AppendToIndex = function () {
      var I = 0;
      var L = 0;
      L = this.FList.length;
      this.FList.length = this.FRows.length;
      for (var $l1 = L, $end2 = this.FRows.length - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FList[I] = I;
      };
    };
    this.Append = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.push(aRecordIndex) - 1;
      return Result;
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      this.FList.splice(aCurrentIndex,0,aRecordIndex);
      Result = aCurrentIndex;
      return Result;
    };
    this.FindRecord = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.indexOf(aRecordIndex);
      return Result;
    };
    this.Update = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      Result = 0;
      if (this.GetRecordIndex(aCurrentIndex) !== aRecordIndex) pas.DB.DatabaseErrorFmt$1("Inconsistent record index in default index, expected %d, got %d.",[aCurrentIndex,this.GetRecordIndex(aCurrentIndex)],this.FDataset);
      return Result;
    };
  });
  rtl.createClass($mod,"TBaseJSONDataSet",pas.DB.TDataSet,function () {
    this.$init = function () {
      pas.DB.TDataSet.$init.call(this);
      this.FOwnsData = false;
      this.FDefaultIndex = null;
      this.FCurrentIndex = null;
      this.FCurrent = 0;
      this.FMetaData = null;
      this.FRows = null;
      this.FDeletedRows = null;
      this.FFieldMapper = null;
      this.FEditIdx = 0;
      this.FEditRow = undefined;
      this.FUseDateTimeFormatFields = false;
    };
    this.$final = function () {
      this.FDefaultIndex = undefined;
      this.FCurrentIndex = undefined;
      this.FMetaData = undefined;
      this.FRows = undefined;
      this.FDeletedRows = undefined;
      this.FFieldMapper = undefined;
      pas.DB.TDataSet.$final.call(this);
    };
    this.SetRows = function (AValue) {
      if (AValue === this.FRows) return;
      this.CheckInactive();
      this.FRows = null;
      this.AddToRows(AValue);
    };
    this.AllocRecordBuffer = function () {
      var Result = new pas.DB.TDataRecord();
      Result.data = new Object();
      Result.bookmark = null;
      Result.state = 0;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
      Buffer.get().data = null;
      Buffer.get().bookmark = null;
      Buffer.get().state = 0;
    };
    this.InternalInitRecord = function (Buffer) {
      Buffer.get().data = this.FFieldMapper.CreateRow();
      Buffer.get().bookmark = null;
      Buffer.get().state = 0;
    };
    this.GetRecord = function (Buffer, GetMode, DoCheck) {
      var Result = 0;
      var BkmIdx = 0;
      Result = 0;
      var $tmp1 = GetMode;
      if ($tmp1 === 1) {
        if (this.FCurrent < (this.FCurrentIndex.GetCount() - 1)) {
          this.FCurrent += 1}
         else Result = 2}
       else if ($tmp1 === 2) {
        if (this.FCurrent > 0) {
          this.FCurrent -= 1}
         else Result = 1}
       else if ($tmp1 === 0) if (this.FCurrent >= this.FCurrentIndex.GetCount()) Result = 2;
      if (Result === 0) {
        BkmIdx = this.FCurrentIndex.GetRecordIndex(this.FCurrent);
        Buffer.get().data = this.FRows[BkmIdx];
        Buffer.get().bookmarkFlag = 0;
        Buffer.get().bookmark = BkmIdx;
        this.CalculateFields(Buffer);
      };
      return Result;
    };
    this.AddToRows = function (AValue) {
      if (this.FRows === null) {
        this.FRows = AValue}
       else {
        this.FRows = this.FRows.concat(AValue);
        this.AppendToIndexes();
      };
    };
    this.InternalClose = function () {
      this.BindFields(false);
      if (this.FDefaultFields) this.DestroyFields();
      this.FreeData();
    };
    this.InternalFirst = function () {
      this.FCurrent = -1;
    };
    this.InternalLast = function () {
      this.FCurrent = this.FCurrentIndex.GetCount();
    };
    this.InternalOpen = function () {
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FFieldMapper = this.CreateFieldMapper();
      if (this.FRows === null) {
        this.FRows = new Array();
        this.FOwnsData = true;
      };
      this.CreateIndexes();
      this.InternalInitFieldDefs();
      if (this.FDefaultFields) this.CreateFields();
      this.BindFields(true);
      this.InitDateTimeFields();
      this.FCurrent = -1;
    };
    this.InternalPost = function () {
      var Idx = 0;
      var B = new pas.DB.TBookmark();
      this.GetBookmarkData(new pas.DB.TDataRecord(this.ActiveBuffer()),{get: function () {
          return B;
        }, set: function (v) {
          B = v;
        }});
      if (this.FState === 3) {
        Idx = this.FRows.push(this.FEditRow) - 1;
        if (this.GetBookmarkFlag(new pas.DB.TDataRecord(this.ActiveBuffer())) === 2) {
          this.FDefaultIndex.Append(Idx);
          if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrentIndex.Append(Idx);
        } else {
          this.FCurrent = this.FDefaultIndex.Insert(this.FCurrent,Idx);
          if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrent = this.FCurrentIndex.Insert(this.FCurrent,Idx);
        };
      } else {
        if (this.FEditIdx === -1) pas.DB.DatabaseErrorFmt("Failed to retrieve record index for record %d",[this.FCurrent]);
        Idx = this.FEditIdx;
        this.FRows[Idx] = this.FEditRow;
        this.FDefaultIndex.Update(this.FCurrent,Idx);
        if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrentIndex.Update(this.FCurrent,Idx);
      };
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalCancel = function () {
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalInitFieldDefs = function () {
      if (this.FMetaData != null) this.MetaDataToFieldDefs();
      if (this.FFieldDefs.GetCount() === 0) throw $mod.EJSONDataset.$create("Create$1",["No fields found"]);
    };
    this.InternalSetToRecord = function (Buffer) {
      this.FCurrent = this.FCurrentIndex.FindRecord(Math.floor(Buffer.bookmark));
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      if (this.FUseDateTimeFormatFields && (FieldType in rtl.createSet(6,8,7))) {
        var $tmp1 = FieldType;
        if ($tmp1 === 6) {
          Result = $mod.TJSONDateField}
         else if ($tmp1 === 8) {
          Result = $mod.TJSONDateTimeField}
         else if ($tmp1 === 7) Result = $mod.TJSONTimeField;
      } else Result = pas.DB.TDataSet.GetFieldClass.call(this,FieldType);
      return Result;
    };
    this.IsCursorOpen = function () {
      var Result = false;
      Result = this.FDefaultIndex != null;
      return Result;
    };
    this.GetBookmarkData = function (Buffer, Data) {
      Data.get().Data = Buffer.bookmark;
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = Buffer.bookmarkFlag;
      return Result;
    };
    this.FreeData = function () {
      if (this.FOwnsData) {
        this.FRows = null;
        this.FMetaData = null;
      };
      if (this.FCurrentIndex !== this.FDefaultIndex) {
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FCurrentIndex;
          }, set: function (v) {
            this.p.FCurrentIndex = v;
          }})}
       else this.FCurrentIndex = null;
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FDefaultIndex;
        }, set: function (v) {
          this.p.FDefaultIndex = v;
        }});
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FCurrentIndex = null;
      this.FDeletedRows = null;
    };
    this.AppendToIndexes = function () {
      this.FDefaultIndex.AppendToIndex();
    };
    this.CreateIndexes = function () {
      this.FDefaultIndex = $mod.TDefaultJSONIndex.$create("Create$1",[this,this.FRows]);
      this.AppendToIndexes();
      this.FCurrentIndex = this.FDefaultIndex;
    };
    this.InitDateTimeFields = function () {
    };
    this.Create$1 = function (AOwner) {
      pas.DB.TDataSet.Create$1.apply(this,arguments);
      this.FOwnsData = true;
      this.FUseDateTimeFormatFields = false;
      this.FEditIdx = -1;
    };
    this.Destroy = function () {
      this.FEditIdx = -1;
      this.FreeData();
      pas.DB.TDataSet.Destroy.apply(this,arguments);
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      var R = undefined;
      if (this.FState in rtl.createSet(5,11)) {
        R = this.FCalcBuffer.data}
       else if (this.FEditIdx == Buffer.bookmark) {
        if (this.FState === 8) {
          R = Buffer.data}
         else R = this.FEditRow;
      } else {
        if (this.FState === 8) {
          return null}
         else R = Buffer.data;
      };
      Result = this.FFieldMapper.GetJSONDataForField$1(Field,R);
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      var R = undefined;
      if (this.FState in rtl.createSet(5,11)) {
        R = this.FCalcBuffer.data}
       else R = this.FEditRow;
      this.FFieldMapper.SetJSONDataForField$1(Field,R,AValue);
      this.SetModified(true);
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      if (rtl.isNumber(Bookmark1.Data) && rtl.isNumber(Bookmark2.Data)) {
        Result = Math.floor(Bookmark2.Data) - Math.floor(Bookmark1.Data)}
       else {
        if (rtl.isNumber(Bookmark1.Data)) {
          Result = -1}
         else if (rtl.isNumber(Bookmark2.Data)) {
          Result = 1}
         else Result = 0;
      };
      return Result;
    };
  });
  rtl.createClass($mod,"TJSONObjectFieldMapper",$mod.TJSONFieldMapper,function () {
    this.SetJSONDataForField = function (FieldName, FieldIndex, Row, Data) {
      rtl.getObject(Row)[FieldName] = Data;
    };
    this.GetJSONDataForField = function (FieldName, FieldIndex, Row) {
      var Result = undefined;
      Result = rtl.getObject(Row)[FieldName];
      return Result;
    };
    this.CreateRow = function () {
      var Result = undefined;
      Result = new Object();
      return Result;
    };
  });
  rtl.createClass($mod,"EJSONDataset",pas.DB.EDatabaseError,function () {
  });
},["DateUtils"]);
rtl.module("StdCtrls",["System","Classes","SysUtils","Types","Web","Graphics","Controls","Forms"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TCustomButton",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FModalResult = 0;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating()) {
        var $with1 = this.FHandleElement;
        $with1.style.setProperty("padding","0");
        $with1.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("button");
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = new pas.Types.TSize();
      Result.cx = 80;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FModalResult = 0;
      this.BeginUpdate();
      try {
        var $with1 = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with1.cx,$with1.cy);
      } finally {
        this.EndUpdate();
      };
    };
    this.AdjustSize = function () {
      var VSize = new pas.Types.TSize();
      pas.Controls.TControl.AdjustSize.call(this);
      VSize = new pas.Types.TSize(this.FFont.TextExtent(this.GetText()));
      this.SetBounds(this.FLeft,this.FTop,VSize.cx,VSize.cy);
    };
    this.Click = function () {
      var VParent = null;
      if (this.FModalResult !== 0) {
        VParent = this.FParent;
        while (VParent != null) {
          if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
            VParent.SetModalResult(this.FModalResult);
            break;
          };
          VParent = VParent.FParent;
        };
      };
      pas.Controls.TControl.Click.call(this);
    };
  });
  rtl.createClass($mod,"TCustomLabel",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = 0;
      this.FContentElement = null;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = false;
      this.FWordWrap = false;
    };
    this.$final = function () {
      this.FContentElement = undefined;
      this.FFocusControl = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetLayout = function (AValue) {
      if (this.FLayout !== AValue) {
        this.FLayout = AValue;
        this.Changed();
      };
    };
    this.SetTransparent = function (AValue) {
      if (this.FTransparent !== AValue) {
        this.FTransparent = AValue;
        this.BeginUpdate();
        try {
          if (this.FTransparent) {
            this.SetColor(536870911);
          } else if (this.FColor === 536870911) {
            this.SetColor(2147483649);
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.DoEnter = function () {
      pas.Controls.TWinControl.DoEnter.call(this);
      if ((this.FFocusControl != null) && this.FFocusControl.CanSetFocus()) {
        this.FFocusControl.SetFocus();
      };
    };
    this.Changed = function () {
      var VTr = null;
      var VTd = null;
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating()) {
        var $with1 = this.FHandleElement;
        if (this.FTransparent) {
          $with1.style.setProperty("background-color","transparent");
        };
        $with1.style.setProperty("outline","none");
        $with1.style.setProperty("user-select","none");
        $with1.style.setProperty("-moz-user-select","none");
        $with1.style.setProperty("-ms-user-select","none");
        $with1.style.setProperty("-khtml-user-select","none");
        $with1.style.setProperty("-webkit-user-select","none");
        var $with2 = this.FContentElement;
        $with2.innerHTML = "";
        $with2.style.setProperty("height","100%");
        $with2.style.setProperty("width","100%");
        $with2.style.setProperty("table-layout","fixed");
        VTr = this.FContentElement.appendChild(document.createElement("tr"));
        VTd = VTr.appendChild(document.createElement("td"));
        var $tmp3 = this.FAlignment;
        if ($tmp3 === 2) {
          VTd.style.setProperty("text-align","center")}
         else if ($tmp3 === 0) {
          VTd.style.setProperty("text-align","left")}
         else if ($tmp3 === 1) VTd.style.setProperty("text-align","right");
        var $tmp4 = this.FLayout;
        if ($tmp4 === 2) {
          VTd.style.setProperty("vertical-align","bottom")}
         else if ($tmp4 === 1) {
          VTd.style.setProperty("vertical-align","middle")}
         else if ($tmp4 === 0) VTd.style.setProperty("vertical-align","top");
        if (this.FWordWrap) {
          VTd.style.setProperty("word-wrap","break-word");
        } else {
          VTd.style.removeProperty("word-wrap");
        };
        VTd.style.setProperty("overflow","hidden");
        VTd.style.setProperty("text-overflow","ellipsis");
        VTd.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateContentElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("table"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = new pas.Types.TSize();
      Result.cx = 65;
      Result.cy = 17;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FContentElement = this.CreateContentElement();
      this.FAlignment = 0;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = true;
      this.FWordWrap = false;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        var $with1 = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with1.cx,$with1.cy);
      } finally {
        this.EndUpdate();
      };
    };
    this.AdjustSize = function () {
      var VSize = new pas.Types.TSize();
      pas.Controls.TControl.AdjustSize.call(this);
      VSize = new pas.Types.TSize(this.FFont.TextExtent(this.GetText()));
      this.SetBounds(this.FLeft,this.FTop,VSize.cx + 4,VSize.cy + 4);
    };
  });
});
rtl.module("WebCtrls",["System","Classes","SysUtils","Types","Graphics","Controls","Forms","StdCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TWForm",pas.Forms.TCustomForm,function () {
    var $r = this.$rtti;
    $r.addProperty("ActiveControl",2,pas.Controls.$rtti["TWinControl"],"FActiveControl","SetActiveControl");
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlphaBlend",2,rtl.boolean,"FAlphaBlend","SetAlphaBlend");
    $r.addProperty("AlphaBlendValue",2,rtl.byte,"FAlphaBlendValue","SetAlphaBlendValue");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("KeyPreview",0,rtl.boolean,"FKeyPreview","FKeyPreview");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnActivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnActivate","FOnActivate");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnClose",0,pas.Forms.$rtti["TCloseEvent"],"FOnClose","FOnClose");
    $r.addProperty("OnCloseQuery",0,pas.Forms.$rtti["TCloseQueryEvent"],"FOnCloseQuery","FOnCloseQuery");
    $r.addProperty("OnCreate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnCreate","FOnCreate");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnDeactivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDeactivate","FOnDeactivate");
    $r.addProperty("OnDestroy",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDestroy","FOnDestroy");
    $r.addProperty("OnHide",0,pas.Classes.$rtti["TNotifyEvent"],"FOnHide","FOnHide");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize$1","FOnResize$1");
    $r.addProperty("OnScroll",0,pas.Classes.$rtti["TNotifyEvent"],"FOnScroll$1","FOnScroll$1");
    $r.addProperty("OnShow",0,pas.Classes.$rtti["TNotifyEvent"],"FOnShow","FOnShow");
  });
  rtl.createClass($mod,"TWButton",pas.StdCtrls.TCustomButton,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("ModalResult",0,pas.Forms.$rtti["TModalResult"],"FModalResult","FModalResult");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass($mod,"TWLabel",pas.StdCtrls.TCustomLabel,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.nativeuint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("FocusControl",0,pas.Controls.$rtti["TWinControl"],"FFocusControl","FFocusControl");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Layout",2,pas.Graphics.$rtti["TTextLayout"],"FLayout","SetLayout");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Transparent",2,rtl.boolean,"FTransparent","SetTransparent");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
});
rtl.module("ucds",["System","Classes","SysUtils","Types","JS","Web","DB","JSONDataset","Controls","WebCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TFieldDataLink",pas.DB.TDataLink,function () {
    this.$init = function () {
      pas.DB.TDataLink.$init.call(this);
      this.FOnDataChange = null;
      this.FOnUpdateData = null;
      this.FOnActiveChange = null;
      this.FField = null;
      this.FFieldName = "";
      this.FModified = false;
    };
    this.$final = function () {
      this.FOnDataChange = undefined;
      this.FOnUpdateData = undefined;
      this.FOnActiveChange = undefined;
      this.FField = undefined;
      pas.DB.TDataLink.$final.call(this);
    };
    this.UpdateField = function () {
      this.FField = null;
      if (((this.FDataSource != null) && (this.FDataSource.FDataSet != null)) && (this.FFieldName !== "")) if (this.FDataSource.FDataSet.GetActive()) this.FField = this.FDataSource.FDataSet.FieldByName(this.FFieldName);
    };
    this.ActiveChanged = function () {
      if (this.FOnActiveChange != null) this.FOnActiveChange(this);
    };
    this.RecordChanged = function (Field) {
      if ((Field === null) || (Field === this.FField)) if (this.FOnDataChange != null) this.FOnDataChange(this);
    };
    this.UpdateData = function () {
      if (this.FModified) {
        if ((this.FField != null) && (this.FOnUpdateData != null)) this.FOnUpdateData(this);
        this.FModified = false;
      };
    };
    var $r = this.$rtti;
    $r.addProperty("FieldName",0,rtl.string,"FFieldName","FFieldName");
    $r.addProperty("OnDataChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDataChange","FOnDataChange");
    $r.addProperty("OnUpdateData",0,pas.Classes.$rtti["TNotifyEvent"],"FOnUpdateData","FOnUpdateData");
    $r.addProperty("OnActiveChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnActiveChange","FOnActiveChange");
  });
  rtl.createClass($mod,"TWDataSource",pas.DB.TDataSource,function () {
    this.$init = function () {
      pas.DB.TDataSource.$init.call(this);
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
    };
    this.$final = function () {
      this.FParent = undefined;
      pas.DB.TDataSource.$final.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Parent",0,pas.Controls.$rtti["TCustomControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
  });
  $mod.$rtti.$ProcVar("TConnectErrorEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["ErrorCode",rtl.longint]])});
  rtl.createClass($mod,"TConnection",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FReq = null;
      this.FDS = null;
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
      this.FActive = false;
      this.FDataNode = "";
      this.FURI = "";
      this.FAfterConnect = null;
      this.FBeforeConnect = null;
      this.FOnConnectError = null;
    };
    this.$final = function () {
      this.FReq = undefined;
      this.FDS = undefined;
      this.FParent = undefined;
      this.FAfterConnect = undefined;
      this.FBeforeConnect = undefined;
      this.FOnConnectError = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetActive = function (AValue) {
      if ((this.FURI !== "") && AValue) {
        this.DoBeforeConnect();
        this.FActive = AValue;
        this.FReq = new XMLHttpRequest();
        this.FReq.addEventListener("load",rtl.createCallback(this,"onLoad"));
        this.FReq.open("GET",this.FURI,true);
        this.FReq.send();
      };
    };
    this.onLoad = function (Event) {
      var Result = false;
      var J = undefined;
      var JA = undefined;
      if (this.FReq.status === 200) {
        J = JSON.parse(this.FReq.responseText);
        JA = rtl.getObject(J)[this.FDataNode];
        this.DoAfterConnect();
        if (this.FDS != null) {
          this.FDS.SetRows(rtl.getObject(JA));
          this.FDS.Open();
        } else this.DoError(this.FReq.status);
      };
      return Result;
    };
    this.RegisterDataSet = function (value) {
      this.FDS = value;
    };
    this.DoBeforeConnect = function () {
      if (this.FBeforeConnect != null) this.FBeforeConnect(this);
    };
    this.DoAfterConnect = function () {
      if (this.FAfterConnect != null) this.FAfterConnect(this);
    };
    this.DoError = function (ErrorCode) {
      if (this.FOnConnectError != null) this.FOnConnectError(this,ErrorCode);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.apply(this,arguments);
      this.FDS = null;
    };
    var $r = this.$rtti;
    $r.addProperty("Parent",0,pas.Controls.$rtti["TCustomControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
    $r.addProperty("Active",2,rtl.boolean,"FActive","SetActive");
    $r.addProperty("DataNode",0,rtl.string,"FDataNode","FDataNode");
    $r.addProperty("URI",0,rtl.string,"FURI","FURI");
    $r.addProperty("AfterConnect",0,pas.Classes.$rtti["TNotifyEvent"],"FAfterConnect","FAfterConnect");
    $r.addProperty("BeforeConnect",0,pas.Classes.$rtti["TNotifyEvent"],"FBeforeConnect","FBeforeConnect");
    $r.addProperty("OnConnectError",0,$mod.$rtti["TConnectErrorEvent"],"FOnConnectError","FOnConnectError");
  });
  rtl.createClass($mod,"TDataSet",pas.JSONDataset.TBaseJSONDataSet,function () {
    this.$init = function () {
      pas.JSONDataset.TBaseJSONDataSet.$init.call(this);
      this.FConnection = null;
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
    };
    this.$final = function () {
      this.FConnection = undefined;
      this.FParent = undefined;
      pas.JSONDataset.TBaseJSONDataSet.$final.call(this);
    };
    this.SetConnection = function (Value) {
      if (Value != null) Value.RegisterDataSet(this);
    };
    this.CreateFieldMapper = function () {
      var Result = null;
      Result = pas.JSONDataset.TJSONObjectFieldMapper.$create("Create");
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Rows",2,pas.JS.$rtti["TJSArray"],"FRows","SetRows");
    $r.addProperty("Parent",0,pas.Controls.$rtti["TCustomControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
    $r.addProperty("Connection",2,$mod.$rtti["TConnection"],"FConnection","SetConnection");
  });
  rtl.createClass($mod,"TDBLabel",pas.WebCtrls.TWLabel,function () {
    this.$init = function () {
      pas.WebCtrls.TWLabel.$init.call(this);
      this.FDataLink = null;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      pas.WebCtrls.TWLabel.$final.call(this);
    };
    this.DataUpdate = function (Sender) {
    };
    this.DataChange = function (Sender) {
      if (!(this.FDataLink.GetDataset() != null)) return;
      if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
      this.SetText(this.FDataLink.FField.GetDisplayText());
    };
    this.ActiveChange = function (Sender) {
      if (this.FDataLink.GetDataset() != null) {
        if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
          this.SetText("")}
         else this.DataChange(this);
      };
    };
    this.SetDataField = function (Value) {
      this.FDataLink.FFieldName = Value;
      this.FDataLink.UpdateField();
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.GetDataField = function () {
      var Result = "";
      Result = this.FDataLink.FFieldName;
      return Result;
    };
    this.Create$1 = function (Owner) {
      pas.StdCtrls.TCustomLabel.Create$1.call(this,Owner);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnUpdateData = rtl.createCallback(this,"DataUpdate");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas.Controls.TControl.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataField",3,rtl.string,"GetDataField","SetDataField");
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
});
rtl.module("Unit1",["System","JS","Web","Classes","SysUtils","Graphics","Controls","Forms","DB","JSONDataset","WebCtrls","ucds"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TWForm1",pas.WebCtrls.TWForm,function () {
    this.$init = function () {
      pas.WebCtrls.TWForm.$init.call(this);
      this.DBLabel1 = null;
      this.FDataLink = null;
      this.Connection1 = null;
      this.DataSet1 = null;
      this.DataSource1 = null;
      this.WButton2 = null;
    };
    this.$final = function () {
      this.DBLabel1 = undefined;
      this.FDataLink = undefined;
      this.Connection1 = undefined;
      this.DataSet1 = undefined;
      this.DataSource1 = undefined;
      this.WButton2 = undefined;
      pas.WebCtrls.TWForm.$final.call(this);
    };
    this.WButton2Click = function (Sender) {
      this.DataSet1.Next();
    };
    this.Loaded = function () {
      pas.Forms.TCustomForm.Loaded.call(this);
      this.BeginUpdate();
      try {
        this.SetLeft(269);
        this.SetHeight(297);
        this.SetTop(250);
        this.SetWidth(378);
        this.SetAlphaBlend(false);
        this.SetAlphaBlendValue(255);
        this.SetText("WForm1");
        this.SetClientHeight(297);
        this.SetClientWidth(378);
        this.WButton2 = pas.WebCtrls.TWButton.$create("Create$1",[this]);
        this.WButton2.BeginUpdate();
        try {
          this.WButton2.SetParent(this);
          this.WButton2.SetLeft(8);
          this.WButton2.SetHeight(25);
          this.WButton2.SetTop(8);
          this.WButton2.SetWidth(72);
          this.WButton2.SetText("WButton2");
          this.WButton2.SetTabOrder(0);
          this.WButton2.FOnClick = rtl.createCallback(this,"WButton2Click");
        } finally {
          this.WButton2.EndUpdate();
        };
      } finally {
        this.EndUpdate();
      };
      this.FDataLink = pas.ucds.TFieldDataLink.$create("Create$1");
      this.Connection1 = pas.ucds.TConnection.$create("Create$1",[this]);
      this.Connection1.SetName("Connection1");
      this.Connection1.SetActive(false);
      this.DataSet1 = pas.ucds.TDataSet.$create("Create$1",[this]);
      this.DataSet1.SetName("DataSet1");
      this.DataSet1.SetConnection(this.Connection1);
      this.DataSource1 = pas.ucds.TWDataSource.$create("Create$1",[this]);
      this.DataSource1.SetName("DataSource1");
      this.DataSource1.SetDataSet(this.DataSet1);
      this.Connection1.FURI = "data.json";
      this.Connection1.FDataNode = "data";
      this.DataSet1.FFieldDefs.Clear();
      this.DataSet1.FFieldDefs.Add$4("Species_No",1,0);
      this.DataSet1.FFieldDefs.Add$4("Category",1,50);
      this.DataSet1.FFieldDefs.Add$4("Common_Name",1,50);
      this.DataSet1.FFieldDefs.Add$4("Species_Name",1,50);
      this.DataSet1.FFieldDefs.Add$4("Length__cm_",2,0);
      this.DataSet1.FFieldDefs.Add$4("Length_In",1,30);
      this.DataSet1.FFieldDefs.Add$4("Notes",1,255);
      this.Connection1.SetActive(true);
      this.DBLabel1 = pas.ucds.TDBLabel.$create("Create$1",[this]);
      this.DBLabel1.SetParent(this);
      this.DBLabel1.SetName("WDBText1");
      this.DBLabel1.SetLeft(90);
      this.DBLabel1.SetTop(7);
      this.DBLabel1.SetWidth(457);
      this.DBLabel1.SetHeight(22);
      this.DBLabel1.SetAutoSize(false);
      this.DBLabel1.SetText("WDBText1");
      this.DBLabel1.SetDataField("Category");
      this.DBLabel1.SetDataSource(this.DataSource1);
    };
  });
  this.WForm1 = null;
});
rtl.module("program",["System","Forms","Unit1","ucds"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    pas.Forms.Application().Initialize();
    pas.Forms.Application().CreateForm(pas.Unit1.TWForm1,{p: pas.Unit1, get: function () {
        return this.p.WForm1;
      }, set: function (v) {
        this.p.WForm1 = v;
      }});
    pas.Forms.Application().Run();
  };
});
//# sourceMappingURL=project1.js.map
