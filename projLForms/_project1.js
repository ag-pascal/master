var pas = {};

var rtl = {

  version: 10101,

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

  checkVersion: function(v){
    if (rtl.version != v) throw "expected rtl version "+v+", but found "+rtl.version;
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
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
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
        o.$init();
        try{
          o[fnname].apply(o,args);
          o.AfterConstruction();
        } catch($e){
          // do not call BeforeDestruction
          if (this.Destroy) this.Destroy();
          this.$final();
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        if (this[fnname]) this[fnname]();
        this.$final();
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
      if (o.$init) o.$init();
      try{
        o[fnname].apply(o,args);
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        // do not call BeforeDestruction
        if (this.Destroy) this.Destroy();
        if (this.$final) this.$final();
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      if (this[fnname]) this[fnname]();
      if (this.$final) this.$final();
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
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
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
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
});
rtl.module("JS",["System"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Web",["System","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("SysUtils",["System","JS"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$ProcVar("TProcedure",{procsig: rtl.newTIProcSig(null)});
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
  });
  this.Trim = function (S) {
    return S.trim();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
  });
  this.FormatSettings = null;
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
  };
});
rtl.module("Classes",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$init = function () {
    $impl.ClassList = Object.create(null);
  };
},["JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ClassList = null;
});
rtl.module("Math",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.Floor = function (A) {
    var Result = 0;
    Result = pas.System.Trunc(Math.floor(A));
    return Result;
  };
});
rtl.module("JComponents",["System","Classes","SysUtils","Math","JS","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$rtti.$MethodVar("TMouseClickEvent",{procsig: rtl.newTIProcSig([["sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass($mod,"TControl",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FName = "";
      this.FTagId = "";
      this.FHandle = null;
      this.FParent = null;
      this.TabOrder = 0;
    };
    this.$final = function () {
      this.FHandle = undefined;
      this.FParent = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.InitializeObject = function () {
    };
    this.createElementTagObj = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.RegisterChild = function (aChild) {
      this.FHandle.appendChild(aChild.FHandle);
    };
    this.Create$1 = function (AOwner) {
      var Self = this;
      function AsObject(aValue) {
        var Result = undefined;
        if ((aValue != undefined) && (aValue != null)) {
          Result = aValue;
        };
        return Result;
      };
      Self.FParent = AOwner;
      pas.System.TObject.Create.call(Self);
      try {
        Self.FTagId = $mod.GenID();
        Self.FHandle = Self.createElementTagObj();
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var e = $e;
          window.console.log(e.fMessage);
        } else throw $e
      };
      if (AsObject(Self.FHandle) != AsObject(window.document.body)) {
        if (Self.FTagId.length > 0) Self.FHandle.setAttribute("id",Self.FTagId);
      };
      if (Self.FParent !== null) Self.FParent.RegisterChild(Self);
      Self.InitializeObject();
    };
    this.Valid = function () {
      var Result = false;
      if (!((this == undefined) || (this == null))) {
        Result = true}
       else Result = false;
      return Result;
    };
    this.ReadyExecute = function (OnReady) {
      if (this.Valid()) {
        window.requestAnimationFrame(OnReady);
      };
    };
    this.BeginUpdate = function () {
    };
    this.EndUpdate = function () {
    };
    var $r = this.$rtti;
    $r.addProperty("Name",0,rtl.string,"FName","FName");
    $r.addProperty("Handle",0,pas.Web.$rtti["TJSHTMLElement"],"FHandle","FHandle");
    $r.addProperty("Parent",0,$r,"FParent","FParent");
    $r.addField("TabOrder",rtl.longint);
  });
  rtl.createClass($mod,"TMovableControl",$mod.TControl,function () {
    this.GetWidth = function () {
      var Result = 0;
      Result = pas.Math.Floor(this.FHandle.offsetWidth);
      return Result;
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = pas.Math.Floor(this.FHandle.offsetHeight);
      return Result;
    };
    this.GetLeft = function () {
      var Result = 0;
      Result = this.setPropertyPosition(this.FHandle,"left");
      return Result;
    };
    this.GetTop = function () {
      var Result = 0;
      Result = this.setPropertyPosition(this.FHandle,"top");
      return Result;
    };
    this.SetLeft = function (aValue) {
      this.setProperty("left",pas.SysUtils.IntToStr(aValue) + "px");
    };
    this.SetTop = function (aValue) {
      this.setProperty("top",pas.SysUtils.IntToStr(aValue) + "px");
    };
    this.SetWidth = function (aValue) {
      this.setProperty("width",pas.SysUtils.IntToStr(aValue) + "px");
    };
    this.SetHeight = function (aValue) {
      this.setProperty("height",pas.SysUtils.IntToStr(aValue) + "px");
    };
    this.setPropertyPosition = function (fTagRef, position) {
      var Result = 0;
      fTagRef = document.defaultView.getComputedStyle(this.FHandle,rtl.getObject(null));
      if (fTagRef != null) Result = parseInt(fTagRef.getPropertyValue(position));
      return Result;
    };
    this.InitializeObject = function () {
      $mod.TControl.InitializeObject.apply(this,arguments);
    };
    this.setProperty = function (propertyName, propertyValue) {
      this.FHandle.style.setProperty(propertyName,propertyValue);
    };
    this.SetBounds = function (aLeft, aTop, aWidth, aHeight) {
      var mSized = false;
      aWidth = Math.max(0,aWidth);
      aHeight = Math.max(0,aHeight);
      mSized = (aWidth !== this.FHandle.offsetWidth) || (aHeight !== this.FHandle.offsetHeight);
      this.setProperty("left",pas.SysUtils.IntToStr(aLeft) + "px");
      this.setProperty("top",pas.SysUtils.IntToStr(aTop) + "px");
      this.setProperty("width",pas.SysUtils.IntToStr(aWidth) + "px");
      this.setProperty("height",pas.SysUtils.IntToStr(aHeight) + "px");
    };
    var $r = this.$rtti;
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
  });
  rtl.createClass($mod,"TCustomControl",$mod.TMovableControl,function () {
    this.$init = function () {
      $mod.TMovableControl.$init.call(this);
      this.FOnClick = null;
    };
    this.$final = function () {
      this.FOnClick = undefined;
      $mod.TMovableControl.$final.call(this);
    };
    this._setMouseClick = function (aValue) {
      this.FOnClick = aValue;
    };
    this.InitializeObject = function () {
      $mod.TMovableControl.InitializeObject.call(this);
      this.FHandle.addEventListener("click",rtl.createCallback(this,"CBClick"));
    };
    this.CBClick = function (eventObj) {
      eventObj.stopPropagation();
      if (this.FOnClick != null) this.FOnClick(this);
    };
    var $r = this.$rtti;
    $r.addProperty("OnClick",2,$mod.$rtti["TMouseClickEvent"],"FOnClick","_setMouseClick");
  });
  rtl.createClass($mod,"TCustomForm",$mod.TCustomControl,function () {
    this.$init = function () {
      $mod.TCustomControl.$init.call(this);
      this.FFormIndex = 0;
      this.FCaption = "";
      this.FInitialized = false;
      this.ClientHeight = 0;
      this.ClientWidth = 0;
    };
    this.LoadDFMValues = function () {
    };
    this.Create$1 = function (AOwner) {
      $mod.TControl.Create$1.call(this,AOwner);
      this.FHandle.classList.add("swiper-slide","TCustomForm");
      this.LoadDFMValues();
    };
    this.FormActivated = function () {
      if (!this.FInitialized) {
        this.FInitialized = true;
        this.InitializeForm();
      };
    };
    this.FormDeactivated = function () {
    };
    this.InitializeForm = function () {
    };
    var $r = this.$rtti;
    $r.addProperty("Caption",0,rtl.string,"FCaption","FCaption");
    $r.addField("ClientHeight",rtl.longint);
    $r.addField("ClientWidth",rtl.longint);
  });
  rtl.createClass($mod,"TWForm",$mod.TCustomForm,function () {
  });
  rtl.createClass($mod,"TCustomApplication",$mod.TCustomForm,function () {
    this.$init = function () {
      $mod.TCustomForm.$init.call(this);
      this.FOldForm = 0;
      this.FForm = null;
      this.FormNames = [];
      this.FormsClasses = [];
      this.FormsInstances = [];
      this.Swiper$1 = null;
    };
    this.$final = function () {
      this.FForm = undefined;
      this.FormNames = undefined;
      this.FormsClasses = undefined;
      this.FormsInstances = undefined;
      this.Swiper$1 = undefined;
      $mod.TCustomForm.$final.call(this);
    };
    this.ApplicationStarting = function () {
    };
    this.Create$3 = function () {
      var docFragment = null;
      var div_ = null;
      var div_0 = null;
      docFragment = document.createDocumentFragment();
      div_ = document.createElement("DIV");
      div_.setAttribute("class","swiper-container");
      div_.setAttribute("style",("width:100%; height:" + pas.SysUtils.IntToStr(window.screen.height)) + "px;");
      docFragment.appendChild(div_);
      div_0 = document.createElement("DIV");
      div_0.setAttribute("class","swiper-wrapper");
      div_0.setAttribute("style","width:100%; height:100%;");
      div_.appendChild(div_0);
      document.body.appendChild(docFragment);
      pas.System.TObject.Create.call(this);
      if (!(this.FForm != null)) this.FForm = $mod.TControl.$create("Create$1",[null]);
      this.FForm.FHandle = document.querySelector(".swiper-container").firstElementChild;
    };
    this.RunApp = function () {
      this.ApplicationStarting();
      if (!(this.Swiper$1 != null)) this.Swiper$1 = new Swiper(".swiper-container");
    };
    this.RegisterAutoCreateForm = function (FormName, aClassType, isMainForm) {
      var k = 0;
      this.FormNames.push(FormName);
      this.FormsClasses.push(aClassType);
      this.FormsInstances.push(null);
      for (var $l1 = 0, $end2 = this.FormNames.length - 1; $l1 <= $end2; $l1++) {
        k = $l1;
        if (this.FormsInstances[k] === null) {
          this.FormsInstances[k] = this.FormsClasses[k].$create("Create$1",[this.FForm]);
          this.FormsInstances[k].FFormIndex = k;
          this.FormsInstances[k].FName = FormName;
          this.FormsInstances[k].FormActivated();
        };
      };
    };
    this.GotoForm = function (aFormName, speed, aCallBackFn) {
      var Self = this;
      var i = 0;
      function IndexOfFormName(aformName) {
        var Result = 0;
        var lcName = "";
        var i = 0;
        lcName = pas.SysUtils.Trim(pas.SysUtils.LowerCase(aformName));
        for (var $l1 = 0, $end2 = Self.FormNames.length - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if (pas.SysUtils.LowerCase(Self.FormsInstances[i].FName) === lcName) return Self.FormsInstances[i].FFormIndex;
        };
        return Result;
      };
      function getForm(index) {
        var Result = null;
        Result = Self.FormsInstances[index];
        return Result;
      };
      var n = "";
      for (var $l1 = 0, $end2 = Self.FormNames.length - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (pas.SysUtils.Trim(pas.SysUtils.LowerCase(Self.FormsInstances[i].FName)) === pas.SysUtils.Trim(pas.SysUtils.LowerCase(getForm(Self.FOldForm).FName))) {
          Self.FormsInstances[i].FormDeactivated();
        };
      };
      if (Self.Swiper$1 != null) $mod.Application.Swiper$1.swipeTo(IndexOfFormName(aFormName),speed,aCallBackFn);
      n = pas.SysUtils.Trim(pas.SysUtils.LowerCase(aFormName));
      for (var $l3 = 0, $end4 = Self.FormNames.length - 1; $l3 <= $end4; $l3++) {
        i = $l3;
        if (pas.SysUtils.Trim(pas.SysUtils.LowerCase(Self.FormsInstances[i].FName)) === pas.SysUtils.Trim(pas.SysUtils.LowerCase(aFormName))) {
          Self.FormsInstances[i].FormActivated();
          Self.FOldForm = Self.FormsInstances[i].FFormIndex;
        };
      };
    };
    var $r = this.$rtti;
    $r.addMethod("GotoForm",0,[["aFormName",rtl.string],["speed",rtl.longint],["aCallBackFn",pas.SysUtils.$rtti["TProcedure"]]]);
    $r.addProperty("CurrentForm",0,rtl.longint,"FOldForm","FOldForm");
  });
  this.GenID = function () {
    var Result = "";
    $impl.__p += 1;
    Result = "J" + pas.SysUtils.IntToStr($impl.__p);
    return Result;
  };
  this.Application = null;
  $mod.$init = function () {
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.__p = 0;
});
rtl.module("uWButton",["System","Classes","SysUtils","JS","Web","JComponents"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TButton",pas.JComponents.TCustomControl,function () {
    this.$init = function () {
      pas.JComponents.TCustomControl.$init.call(this);
      this.FLeft = "";
      this.FTop = "";
      this.FWidth = "";
      this.FHeight = "";
      this.FEnabled = false;
    };
    this.GetCaption = function () {
      var Result = "";
      Result = this.FHandle.innerText;
      return Result;
    };
    this.SetCaption = function (Value) {
      this.FHandle.innerText = Value;
    };
    this.InitializeObject = function () {
      var Self = this;
      pas.JComponents.TCustomControl.InitializeObject.apply(Self,arguments);
      Self.FHandle.classList.add("button");
    };
    this.createElementTagObj = function () {
      var Result = null;
      Result = document.createElement("button");
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.JComponents.TControl.Create$1.call(this,AOwner);
    };
    var $r = this.$rtti;
    $r.addProperty("_Left",0,rtl.string,"FLeft","FLeft");
    $r.addProperty("_Top",0,rtl.string,"FTop","FTop");
    $r.addProperty("_Width",0,rtl.string,"FWidth","FWidth");
    $r.addProperty("_Height",0,rtl.string,"FHeight","FHeight");
    $r.addProperty("CustomSize",0,rtl.boolean,"FEnabled","FEnabled");
    $r.addProperty("Caption",3,rtl.string,"GetCaption","SetCaption");
  });
});
rtl.module("UForm1",["System","Classes","SysUtils","JS","Web","JComponents","uWButton"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TForm1",pas.JComponents.TWForm,function () {
    this.$init = function () {
      pas.JComponents.TWForm.$init.call(this);
      this.button1 = null;
    };
    this.$final = function () {
      this.button1 = undefined;
      pas.JComponents.TWForm.$final.call(this);
    };
    this.InitializeForm = function () {
      pas.JComponents.TCustomForm.InitializeForm.apply(this,arguments);
      window.console.log("TForm1.InitializeForm");
    };
    this.InitializeObject = function () {
      var Self = this;
      function Button1OnClick(sender) {
        window.console.log("TForm1 button1 clicked");
        pas.JComponents.Application.GotoForm("Form2",350,null);
      };
      function rolaPreta(aTime) {
        window.console.log("Form1",aTime);
      };
      pas.JComponents.TCustomControl.InitializeObject.apply(Self,arguments);
      window.console.log("TForm1.InitializeObject");
      Self.button1 = pas.uWButton.TButton.$create("Create$1",[Self]);
      Self.button1.SetBounds(10,30,150,150);
      Self.button1.SetCaption("Btn1");
      Self.button1._setMouseClick(Button1OnClick);
      Self.button1.ReadyExecute(rolaPreta);
    };
  });
});
rtl.module("UForm2",["System","Classes","SysUtils","JS","Web","JComponents","uWButton"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TForm2",pas.JComponents.TWForm,function () {
    this.$init = function () {
      pas.JComponents.TWForm.$init.call(this);
      this.button1 = null;
    };
    this.$final = function () {
      this.button1 = undefined;
      pas.JComponents.TWForm.$final.call(this);
    };
    this.InitializeForm = function () {
      pas.JComponents.TCustomForm.InitializeForm.apply(this,arguments);
      window.console.log("TForm2.InitializeForm");
    };
    this.InitializeObject = function () {
      var Self = this;
      function Button1OnClick(sender) {
        window.console.log("TForm2 button clicked");
        pas.JComponents.Application.GotoForm("Form4",350,null);
      };
      function rolaPreta(aTime) {
        window.console.log("Form2",aTime);
        Self.button1.FHandle.style.setProperty("color","white");
        Self.button1.FHandle.style.setProperty("color","white");
        Self.button1.FHandle.style.setProperty("border-radius","4px");
        Self.button1.FHandle.style.setProperty("background","#699BCE");
        Self.button1.FHandle.style.setProperty("cursor","pointer");
        Self.button1.FHandle.style.setProperty("box-shadow","0 -1px 1px 0 rgba(0, 0, 0, 0.25) inset, 0 1px 1px 0 rgba(0, 0, 0, 0.10) inset;)");
      };
      pas.JComponents.TCustomControl.InitializeObject.apply(Self,arguments);
      window.console.log("TForm2.InitializeObject");
      Self.button1 = pas.uWButton.TButton.$create("Create$1",[Self]);
      Self.button1.SetBounds(20,20,200,200);
      Self.button1.SetCaption("Btn2");
      Self.button1._setMouseClick(Button1OnClick);
      Self.button1.ReadyExecute(rolaPreta);
    };
  });
});
rtl.module("UForm3",["System","Classes","SysUtils","JS","Web","JComponents","uWButton"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TForm3",pas.JComponents.TWForm,function () {
    this.$init = function () {
      pas.JComponents.TWForm.$init.call(this);
      this.button1 = null;
    };
    this.$final = function () {
      this.button1 = undefined;
      pas.JComponents.TWForm.$final.call(this);
    };
    this.InitializeForm = function () {
      pas.JComponents.TCustomForm.InitializeForm.apply(this,arguments);
      window.console.log("TForm3.InitializeForm");
    };
    this.InitializeObject = function () {
      var Self = this;
      function Button1OnClick(sender) {
        window.console.log("TForm3 button3 clicked");
        pas.JComponents.Application.GotoForm("Form1",350,null);
      };
      function rolaPreta(aTime) {
        window.console.log("Form3",aTime);
        Self.button1.FHandle.style.setProperty("width","calc(100% - 20px)");
        Self.button1.FHandle.style.setProperty("height","50px");
      };
      pas.JComponents.TCustomControl.InitializeObject.apply(Self,arguments);
      window.console.log("TForm3.InitializeObject");
      Self.button1 = pas.uWButton.TButton.$create("Create$1",[Self]);
      Self.button1.SetBounds(0,30,250,250);
      Self.button1.SetCaption("Btn3");
      Self.button1._setMouseClick(Button1OnClick);
      Self.button1.ReadyExecute(rolaPreta);
    };
  });
});
rtl.module("UForm4",["System","Classes","SysUtils","JS","Web","JComponents","uWButton"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TForm4",pas.JComponents.TWForm,function () {
    this.$init = function () {
      pas.JComponents.TWForm.$init.call(this);
      this.Btn1 = null;
      this.button1 = null;
    };
    this.$final = function () {
      this.Btn1 = undefined;
      this.button1 = undefined;
      pas.JComponents.TWForm.$final.call(this);
    };
    this.Btn1Click = function (Sender) {
      window.console.log("TForm4.Btn1Click");
      pas.JComponents.Application.GotoForm("Form3",350,null);
    };
    this.InitializeForm = function () {
      pas.JComponents.TCustomForm.InitializeForm.apply(this,arguments);
      window.console.log("TForm4.InitializeForm");
    };
    this.InitializeObject = function () {
      var Self = this;
      function Button1OnClick(sender) {
        window.console.log("TForm4 button1 clicked");
        pas.JComponents.Application.GotoForm("Form1",350,null);
      };
      function rolaPreta(aTime) {
        window.console.log("Form4",aTime);
      };
      pas.JComponents.TCustomControl.InitializeObject.apply(Self,arguments);
      window.console.log("TForm4.InitializeObject");
      Self.button1 = pas.uWButton.TButton.$create("Create$1",[Self]);
      Self.button1.SetBounds(10,30,100,50);
      Self.button1.SetCaption("Btn4");
      Self.button1._setMouseClick(Button1OnClick);
      Self.button1.ReadyExecute(rolaPreta);
    };
    this.LoadDFMValues = function () {
      pas.JComponents.TCustomForm.LoadDFMValues.apply(this,arguments);
      this.Btn1 = pas.uWButton.TButton.$create("Create$1",[this]);
      this.Btn1.BeginUpdate();
      try {
        this.FName = "Form4";
        this.SetLeft(0);
        this.SetHeight(768);
        this.SetTop(0);
        this.SetWidth(1024);
        this.FCaption = "Form4";
        this.ClientHeight = 768;
        this.ClientWidth = 1024;
        var $with1 = this.Btn1;
        $with1.FName = "Btn1";
        $with1.FParent = this;
        $with1.SetLeft(101);
        $with1.SetHeight(47);
        $with1.SetTop(88);
        $with1.SetWidth(120);
        $with1.SetCaption("goTo 3");
        $with1._setMouseClick(rtl.createCallback(this,"Btn1Click"));
        $with1.TabOrder = 0;
      } finally {
        this.Btn1.EndUpdate();
      };
    };
  });
});
rtl.module("uMain",["System","Classes","SysUtils","JS","Web","JComponents","UForm1","UForm2","UForm3","UForm4"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TApp",pas.JComponents.TCustomApplication,function () {
    this.ApplicationStarting = function () {
      window.console.log("TApp.ApplicationStarting");
      pas.JComponents.Application.RegisterAutoCreateForm("Form1",pas.UForm1.TForm1,true);
      pas.JComponents.Application.RegisterAutoCreateForm("Form2",pas.UForm2.TForm2,false);
      pas.JComponents.Application.RegisterAutoCreateForm("Form3",pas.UForm3.TForm3,false);
      pas.JComponents.Application.RegisterAutoCreateForm("Form4",pas.UForm4.TForm4,false);
      pas.JComponents.Application.GotoForm("Form1",350,null);
      pas.JComponents.TCustomApplication.ApplicationStarting.apply(this,arguments);
    };
  });
});
rtl.module("program",["System","Web","Classes","SysUtils","JComponents","uMain"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    try {
      pas.JComponents.Application = pas.uMain.TApp.$create("Create$3");
      pas.JComponents.Application.RunApp();
    } catch ($e) {
      if (pas.SysUtils.Exception.isPrototypeOf($e)) {
        var e = $e;
        window.console.log(e.fMessage);
      } else throw $e
    };
  };
});
//# sourceMappingURL=project1.js.map
