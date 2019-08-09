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
  var $impl = $mod.$impl;
  this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
    };
    this.Destroy = function () {
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
    Result = $mod.Trunc(A);
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
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
    Result = "";
    for (var $l1 = 1, $end2 = l; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + c;
    };
    return Result;
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = rtl.length(arguments) - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l1 = 0, $end2 = l; $l1 <= $end2; $l1++) {
        i = $l1;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l3 = 0, $end4 = l; $l3 <= $end4; $l3++) {
        i = $l3;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.WriteBuf = "";
  $impl.WriteCallBack = null;
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.TJSValueType = {"0": "jvtNull", jvtNull: 0, "1": "jvtBoolean", jvtBoolean: 1, "2": "jvtInteger", jvtInteger: 2, "3": "jvtFloat", jvtFloat: 3, "4": "jvtString", jvtString: 4, "5": "jvtObject", jvtObject: 5, "6": "jvtArray", jvtArray: 6};
  this.GetValueType = function (JS) {
    var Result = 0;
    var t = "";
    if ($mod.isNull(JS)) {
      Result = $mod.TJSValueType.jvtNull}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = $mod.TJSValueType.jvtString}
       else if (t === "boolean") {
        Result = $mod.TJSValueType.jvtBoolean}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = $mod.TJSValueType.jvtArray}
         else Result = $mod.TJSValueType.jvtObject;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = $mod.TJSValueType.jvtInteger}
       else Result = $mod.TJSValueType.jvtFloat;
    };
    return Result;
  };
});
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SListCapacityError = "List capacity (%s) exceeded.";
  this.SListIndexError = "List index (%s) out of bounds";
  this.SSortedListError = "Operation not allowed on sorted list";
  this.SDuplicateString = "String list does not allow duplicates";
  this.SErrFindNeedsSortedList = "Cannot use find on unsorted list";
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
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
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
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
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
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffGeneral,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffCurrency,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(pas.JS.TJSValueType.jvtString,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
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
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
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
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === $mod.TFloatFormat.ffGeneral) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffExponent) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffFixed) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffNumber) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator)}
     else if ($tmp1 === $mod.TFloatFormat.ffCurrency) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,DS,$mod.ThousandSeparator);
    if (((format !== $mod.TFloatFormat.ffCurrency) && (Result.length > 1)) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    return Result;
  };
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
  });
  this.FormatSettings = null;
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
  rtl.createClass($mod,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass($mod,"EStringListError",$mod.EListError,function () {
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
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass($mod,"TStrings",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FSpecialCharsInited = false;
      this.FAlwaysQuote = false;
      this.FQuoteChar = "";
      this.FDelimiter = "";
      this.FNameValueSeparator = "";
      this.FUpdateCount = 0;
      this.FLBS = 0;
      this.FLineBreak = "";
    };
    this.Error = function (Msg, Data) {
      throw $mod.EStringListError.$create("CreateFmt",[Msg,[pas.SysUtils.IntToStr(Data)]]);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.GetCount();
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      if (Index === 0) ;
      Result = null;
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) ;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FAlwaysQuote = false;
    };
    this.Destroy = function () {
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      Result = this.GetCount();
      this.Insert(this.GetCount(),S);
      return Result;
    };
    this.AddObject = function (S, AObject) {
      var Result = 0;
      Result = this.Add(S);
      this.PutObject(Result,AObject);
      return Result;
    };
    this.AddStrings = function (TheStrings) {
      var Runner = 0;
      for (var $l1 = 0, $end2 = TheStrings.GetCount() - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        this.AddObject(TheStrings.Get(Runner),TheStrings.GetObject(Runner));
      };
    };
    this.Assign = function (Source) {
      var S = null;
      if ($mod.TStrings.isPrototypeOf(Source)) {
        S = Source;
        this.BeginUpdate();
        try {
          this.Clear();
          this.FSpecialCharsInited = S.FSpecialCharsInited;
          this.FQuoteChar = S.FQuoteChar;
          this.FDelimiter = S.FDelimiter;
          this.FNameValueSeparator = S.FNameValueSeparator;
          this.FLBS = S.FLBS;
          this.FLineBreak = S.FLineBreak;
          this.AddStrings(S);
        } finally {
          this.EndUpdate();
        };
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      if (this.FUpdateCount === 0) this.SetUpdateState(true);
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.SetUpdateState(false);
    };
  });
  this.TStringItem = function (s) {
    if (s) {
      this.FString = s.FString;
      this.FObject = s.FObject;
    } else {
      this.FString = "";
      this.FObject = null;
    };
    this.$equal = function (b) {
      return (this.FString === b.FString) && (this.FObject === b.FObject);
    };
  };
  this.TStringsSortStyle = {"0": "sslNone", sslNone: 0, "1": "sslUser", sslUser: 1, "2": "sslAuto", sslAuto: 2};
  rtl.createClass($mod,"TStringList",$mod.TStrings,function () {
    this.$init = function () {
      $mod.TStrings.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FOnChange = null;
      this.FOnChanging = null;
      this.FDuplicates = 0;
      this.FCaseSensitive = false;
      this.FOwnsObjects = false;
      this.FSortStyle = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FOnChange = undefined;
      this.FOnChanging = undefined;
      $mod.TStrings.$final.call(this);
    };
    this.GetSorted = function () {
      var Result = false;
      Result = this.FSortStyle in rtl.createSet($mod.TStringsSortStyle.sslUser,$mod.TStringsSortStyle.sslAuto);
      return Result;
    };
    this.Grow = function () {
      var NC = 0;
      NC = this.GetCapacity();
      if (NC >= 256) {
        NC = NC + Math.floor(NC / 4)}
       else if (NC === 0) {
        NC = 4}
       else NC = NC * 4;
      this.SetCapacity(NC);
    };
    this.InternalClear = function (FromIndex, ClearOnly) {
      var I = 0;
      if (FromIndex < this.FCount) {
        if (this.FOwnsObjects) {
          for (var $l1 = FromIndex, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
            I = $l1;
            this.FList[I].FString = "";
            pas.SysUtils.FreeAndNil({p: this.FList[I], get: function () {
                return this.p.FObject;
              }, set: function (v) {
                this.p.FObject = v;
              }});
          };
        } else {
          for (var $l3 = FromIndex, $end4 = this.FCount - 1; $l3 <= $end4; $l3++) {
            I = $l3;
            this.FList[I].FString = "";
          };
        };
        this.FCount = FromIndex;
      };
      if (!ClearOnly) this.SetCapacity(0);
    };
    this.CheckIndex = function (AIndex) {
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.Error(pas.RTLConsts.SListIndexError,AIndex);
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) {
        if (this.FOnChange != null) this.FOnChange(this);
      };
    };
    this.Changing = function () {
      if (this.FUpdateCount === 0) if (this.FOnChanging != null) this.FOnChanging(this);
    };
    this.Get = function (Index) {
      var Result = "";
      this.CheckIndex(Index);
      Result = this.FList[Index].FString;
      return Result;
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = rtl.length(this.FList);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FCount;
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      this.CheckIndex(Index);
      Result = this.FList[Index].FObject;
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FObject = AObject;
      this.Changed();
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < 0) this.Error(pas.RTLConsts.SListCapacityError,NewCapacity);
      if (NewCapacity !== this.GetCapacity()) this.FList = rtl.arraySetLength(this.FList,$mod.TStringItem,NewCapacity);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) {
        this.Changing()}
       else this.Changed();
    };
    this.InsertItem = function (Index, S) {
      this.InsertItem$1(Index,S,null);
    };
    this.InsertItem$1 = function (Index, S, O) {
      var It = new $mod.TStringItem();
      this.Changing();
      if (this.FCount === this.GetCapacity()) this.Grow();
      It.FString = S;
      It.FObject = O;
      this.FList.splice(Index,0,It);
      this.FCount += 1;
      this.Changed();
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      if (this.FCaseSensitive) {
        Result = pas.SysUtils.CompareStr(s1,s2)}
       else Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.Destroy = function () {
      this.InternalClear(0,false);
      $mod.TStrings.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      if (!(this.FSortStyle === $mod.TStringsSortStyle.sslAuto)) {
        Result = this.FCount}
       else if (this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) {
        var $tmp1 = this.FDuplicates;
        if ($tmp1 === pas.Types.TDuplicates.dupIgnore) {
          return Result}
         else if ($tmp1 === pas.Types.TDuplicates.dupError) this.Error(pas.RTLConsts.SDuplicateString,0);
      };
      this.InsertItem(Result,S);
      return Result;
    };
    this.Clear = function () {
      if (this.FCount === 0) return;
      this.Changing();
      this.InternalClear(0,false);
      this.Changed();
    };
    this.Find = function (S, Index) {
      var Result = false;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      Result = false;
      Index.set(-1);
      if (!this.GetSorted()) throw $mod.EListError.$create("Create$1",[pas.RTLConsts.SErrFindNeedsSortedList]);
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + Math.floor((R - L) / 2);
        CompareRes = this.DoCompareText(S,this.FList[I].FString);
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            Result = true;
            if (this.FDuplicates !== pas.Types.TDuplicates.dupAccept) L = I;
          };
        };
      };
      Index.set(L);
      return Result;
    };
    this.Insert = function (Index, S) {
      if (this.FSortStyle === $mod.TStringsSortStyle.sslAuto) {
        this.Error(pas.RTLConsts.SSortedListError,0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.Error(pas.RTLConsts.SListIndexError,Index);
        this.InsertItem(Index,S);
      };
    };
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
rtl.module("uutils",["System","Classes","SysUtils","Types","JS","Web"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Unit1",["System","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FTranslations = null;
    };
    this.$final = function () {
      this.FTranslations = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.SetTranslations = function (Value) {
      this.FTranslations.Assign(Value);
    };
    this.SetupTranslations = function () {
      this.FTranslations.BeginUpdate();
      try {
        this.FTranslations.Add("DuplicateMsg=An item with key %0:s already exists");
        this.FTranslations.Add("TYPE_UNKNOWN=Unknown");
        this.FTranslations.Add("TYPE_STRING=String");
        this.FTranslations.Add("TYPE_BOOLEAN=Boolean");
        this.FTranslations.Add("TYPE_INTEGER=Integer");
        this.FTranslations.Add("TYPE_FLOAT=Float");
        this.FTranslations.Add("TYPE_DATE=Date");
        this.FTranslations.Add("TYPE_TIME=Time");
        this.FTranslations.Add("TYPE_DATETIME=DateTime");
        this.FTranslations.Add("TYPE_BLOB=Blob");
        this.FTranslations.Add("TYPE_SYMBOL=Symbol");
        this.FTranslations.Add("DLG_MSG=Message");
        this.FTranslations.Add("DLG_BTN_OK=OK");
        this.FTranslations.Add("DLG_BTN_CANCEL=Cancel");
        this.FTranslations.Add("DLG_BTN_ABORT=Abort");
        this.FTranslations.Add("DLG_BTN_RETRY=Retry");
        this.FTranslations.Add("DLG_BTN_IGNORE=Ignore");
        this.FTranslations.Add("DLG_BTN_YES=Yes");
        this.FTranslations.Add("DLG_BTN_NO=No");
        this.FTranslations.Add("DLG_BTN_ALL=All");
        this.FTranslations.Add("DLG_BTN_NOTOALL=No to All");
        this.FTranslations.Add("DLG_BTN_YESTOALL=Yes to All");
        this.FTranslations.Add("DLG_BTN_CLOSE=Close");
        this.FTranslations.Add('ERR_CMP_DESTROY="%s" component already destroyed');
        this.FTranslations.Add("ERR_LOAD_PERSISTENT=Persistent load error (%s)");
        this.FTranslations.Add("ERR_LOAD_METHOD=Method %s not found");
        this.FTranslations.Add("ERR_SAVE_PERSISTENT=Persistent save error (%s)");
        this.FTranslations.Add('ERR_SET_RANGE=Value "%s" out of range for set');
        this.FTranslations.Add('ERR_BOOLEAN_LITERAL=Invalid boolean literal "%s" specified');
        this.FTranslations.Add("ERR_FORMAT=Error in the format string %s (%s)");
        this.FTranslations.Add("ERR_VALUE_CONVERT=Error converting %s value to %s value");
        this.FTranslations.Add('ERR_VALUE_READONLY="%s" is read-only and cannot be modified');
        this.FTranslations.Add("ERR_DATETIME_DATE=date");
        this.FTranslations.Add("ERR_DATETIME_TIME=time");
        this.FTranslations.Add("ERR_DATETIME_MONTH=Invalid month %s specified");
        this.FTranslations.Add("ERR_DATETIME_DAY=Invalid day %s specified");
        this.FTranslations.Add("ERR_DATETIME_TOOMANYCOMPS=Too many %s components");
        this.FTranslations.Add("ERR_DATETIME_MISSINGCOMP=Missing %s component");
        this.FTranslations.Add("ERR_DATETIME_INVALIDCOMP=Invalid %s component");
        this.FTranslations.Add("ERR_DATETIME_INVALIDFORMAT=Invalid %s format");
        this.FTranslations.Add("ERR_DATETIME_INVALID=Invalid %s (%s)");
        this.FTranslations.Add("ERR_PARSE_TERMSTR=Unterminated string at %s");
        this.FTranslations.Add("ERR_PARSE_MISSING=Missing %s");
        this.FTranslations.Add("ERR_PARSE_EXPECT=Expected %s, instead found %s at %s");
        this.FTranslations.Add("ERR_LIST_BOUNDS=List index %s out of bounds");
        this.FTranslations.Add("ERR_LIST_SORT=You can only use the Find method when a list is sorted");
        this.FTranslations.Add("ERR_OWNER=Invalid owner class %s passed to constructor");
        this.FTranslations.Add("ERR_LOADUI_STATE=Error loading interface state (%s)");
        this.FTranslations.Add("ERR_UI_DUPSTATE=The interface state %s already exists");
        this.FTranslations.Add("ERR_UI_ELEMENTCLASS=Cannot find registered element class information " + "for the %s class");
        this.FTranslations.Add('ERR_DOM_EVENTADD=Cannot add event handler to "%s" element for "%s" event');
        this.FTranslations.Add('ERR_DOM_EVENTCLEAR=Cannot remove "%s" event handler from "%s"');
        this.FTranslations.Add('ERR_HTTP_REQUEST=Error executing request "%s" (%s)');
        this.FTranslations.Add('ERR_DATA_DUPCOL=The "%s" column already exists');
        this.FTranslations.Add("ERR_DATA_COLNAME=Column names cannot be blank (%s)");
        this.FTranslations.Add("ERR_DATA_COLTYPE=Unknown column type (%s)");
        this.FTranslations.Add('ERR_DATA_COLLENGTH=Invalid "%s" column length %s');
        this.FTranslations.Add('ERR_DATA_COLSCALE=Invalid "%s" column scale %s');
        this.FTranslations.Add("ERR_DATA_CONNECT=Cannot connect to server");
        this.FTranslations.Add("ERR_DATA_LOADCODE=Status code %s");
        this.FTranslations.Add("ERR_DATA_LOAD=Dataset load response error (%s)");
        this.FTranslations.Add("ERR_DATA_COMMIT=Database commit response error (%s)");
        this.FTranslations.Add("ERR_DATA_TRANSACTIVE=A transaction is not active");
        this.FTranslations.Add("ERR_DATA_PENDREQUEST=There are no pending requests");
        this.FTranslations.Add('ERR_DATA_COLUMNS=At least one column must be defined for the "%s" dataset');
        this.FTranslations.Add('ERR_DATA_OPEN=The "%s" dataset must be open in order to complete this operation');
        this.FTranslations.Add('ERR_DATA_NOTOPEN=The "%s" dataset cannot be open when completing this operation');
        this.FTranslations.Add('ERR_DATA_NOTEDITING=The "%s" dataset must be in an editable mode before a column ' + "can be assigned a value");
        this.FTranslations.Add('ERR_DATA_TRANSCLOSE=Cannot close the "%s" dataset while there are still ' + "active transaction operations for the dataset");
        this.FTranslations.Add('ERR_DATA_FINDMODE=The "%s" dataset is not in Find mode');
        this.FTranslations.Add('ERR_DATA_FINDNEAR=You can only search for nearest matches in the "%s" dataset ' + "when searching on columns that match the current sort order");
        this.FTranslations.Add('ERR_DATA_COLNOTFOUND=Column "%s" not found');
        this.FTranslations.Add('ERR_DATA_TRANSDATASET=Invalid dataset "%s" specified in the transaction operations');
        this.FTranslations.Add("ERR_DATA_TRANSOPTYPE=Invalid or unknown operation type %s specified in the transaction operations");
        this.FTranslations.Add('ERR_DATA_DATASETDB=The "%s" dataset cannot be loaded using the "%s" database');
        this.FTranslations.Add("ERR_APP_ERRORLINE=Line: %s");
        this.FTranslations.Add("ERR_APP_ERRORTITLE=Application Error");
        this.FTranslations.Add("APP_LOAD_MESSAGE=Loading %s...");
        this.FTranslations.Add("ERR_DLG_BUTTONS=You must specify at least one button for the message dialog");
        this.FTranslations.Add("ERR_FORM_SHOWMODAL=You cannot call ShowModal for the embedded form %s");
        this.FTranslations.Add("ERR_CTRL_PARENT=The %s control cannot be a parent of the %s control");
        this.FTranslations.Add("ERR_CALENDAR_COLINDEX=Column index %s out of bounds");
        this.FTranslations.Add("ERR_CALENDAR_ROWINDEX=Row index %s out of bounds");
        this.FTranslations.Add("ERR_GRID_COLINDEX=Column index %s out of bounds");
        this.FTranslations.Add("ERR_GRID_ROWINDEX=Row index %s out of bounds");
        this.FTranslations.Add('ERR_GRID_COLNOTFOUND=Column "%s" not found');
        this.FTranslations.Add("ERR_CANVAS=Your browser does not have HTML5 canvas support");
        this.FTranslations.Add("ERR_STORAGE=Your browser does not have HTML5 persistent storage support");
        this.FTranslations.Add("ERR_SCRIPT_LOAD=Your browser does not support dynamic script loading");
        this.FTranslations.Add("ERR_MEDIA=Your browser does not have HTML5 media support");
        this.FTranslations.Add("ERR_MAP=The map API has not been loaded");
        this.FTranslations.Add('ERR_MAP_GEOCODE=Geocoding request error "%s"');
        this.FTranslations.Add('ERR_MAP_LOCNOTFOUND=Location "%s" not found');
        this.FTranslations.Add('ERR_MAP_DUPLOC=The "%s" location already exists');
        this.FTranslations.Add("ERR_MAP_LOCNAME=Location names cannot be blank (%s)");
        this.FTranslations.Add("ERR_SIZER_CONTROL=The sizer control itself cannot be assigned as the target control");
        this.FTranslations.Add("ERR_ZOOM_FACTOR=Zoom factor %s invalid, factor must be between 1 and 100");
        this.FTranslations.Add("ERR_SLIDE_COUNT=At least %s slide images must be specifed before the slide " + "show can be started");
      } finally {
        this.FTranslations.EndUpdate();
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FTranslations = pas.Classes.TStringList.$create("Create$1");
      this.SetupTranslations();
    };
    var $r = this.$rtti;
    $r.addProperty("Translations",2,pas.Classes.$rtti["TStrings"],"FTranslations","SetTranslations");
  });
  this.FormatSettings = null;
  $mod.$resourcestrings = {DuplicateMsg: {org: "An item with key %0:s already exists"}, KeyNotFoundMsg: {org: "Method: %0:s key ['%1:s'] not found in container"}, NotEmptyMsg: {org: "Hash table not empty."}, SErrNoSuchItem: {org: "No item in list for %p"}, SDuplicateItem: {org: "Item already exists in list: %p"}};
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create$1");
  };
});
rtl.module("uinternationalization",["System","JS","Web","Types","Classes","SysUtils","uutils","Unit1"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TJSi18n",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.xhttp = null;
      this.langDocument = undefined;
    };
    this.$final = function () {
      this.xhttp = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.init = function () {
      var Self = this;
      var tmp = null;
      var languages = null;
      var i = 0;
      function languageCallBack(event) {
        Self.switchLanguage(event.target.dataset["lang"]);
      };
      function languageforEach(value, index, anArray) {
        var Result = false;
        rtl.getObject(languages[index]).addEventListener("click",languageCallBack,false);
        return Result;
      };
      tmp = document.getElementsByClassName("language");
      languages = new Array();
      for (var $l1 = 0, $end2 = tmp.length - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        languages.push(tmp.item(i));
      };
      languages.forEach(languageforEach);
    };
    this.switchLanguage = function (language) {
      this.xhttp = new XMLHttpRequest();
      this.xhttp.addEventListener("load",rtl.createCallback(this,"DoFormLoad"));
      this.xhttp.addEventListener("abort",rtl.createCallback(this,"DoFormAbort"));
      this.xhttp.open("GET",("i18n\/" + ("" + language)) + ".json",true);
      this.xhttp.setRequestHeader("Content-type","application\/json");
      this.xhttp.setRequestHeader("Cache-Control","no-cache");
      this.xhttp.send();
    };
    this.processLangDocument = function () {
      var Self = this;
      var tags = null;
      var l = null;
      var i = 0;
      function LforEach(value, index, anArray) {
        var Result = false;
        var key = "";
        key = "" + rtl.getObject(value).dataset["languagekey"];
        if (rtl.getObject(Self.langDocument)[key]) rtl.getObject(l[index]).innerText = "" + rtl.getObject(Self.langDocument)[key];
        return Result;
      };
      tags = document.querySelectorAll("span,img,a,label,li,option,h1,h2,h3,h4,h5,h6");
      l = new Array();
      for (var $l1 = 0, $end2 = tags.length - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        l.push(tags.item(i));
      };
      l.forEach(LforEach);
    };
    this.Create$1 = function () {
    };
    this.DoFormLoad = function (Event) {
      var Result = false;
      if ((this.xhttp.status === 200) && (this.xhttp.readyState === 4)) {
        this.langDocument = JSON.parse(this.xhttp.responseText);
        this.processLangDocument();
      };
      Result = true;
      return Result;
    };
    this.DoFormAbort = function (Event) {
      var Result = false;
      pas.System.Writeln("Failed to load form HTML template file");
      Result = true;
      return Result;
    };
    this.InitializeObject = function () {
      window.console.log("i18n1.InitializeObject");
      this.init();
    };
    var $r = this.$rtti;
    $r.addMethod("InitializeObject",0,null);
  });
});
rtl.module("uMain",["System","uinternationalization","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TApplication",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.i18n1 = null;
    };
    this.$final = function () {
      this.i18n1 = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.RunApp = function () {
      this.i18n1 = pas.uinternationalization.TJSi18n.$create("Create$1");
      this.i18n1.InitializeObject();
    };
  });
});
rtl.module("program",["System","uMain","Web","Classes","SysUtils","uutils"],function () {
  "use strict";
  var $mod = this;
  this.Application = null;
  $mod.$main = function () {
    try {
      $mod.Application = pas.uMain.TApplication.$create("Create");
      $mod.Application.RunApp();
    } catch ($e) {
      if (pas.SysUtils.Exception.isPrototypeOf($e)) {
        var e = $e;
        window.console.log(e.fMessage);
      } else throw $e
    };
  };
});
//# sourceMappingURL=project1.js.map
