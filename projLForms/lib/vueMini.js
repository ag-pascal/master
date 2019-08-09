(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define([], factory);
	else {
		var a = factory();
		for(var i in a) (typeof exports === 'object' ? exports : root)[i] = a[i];
	}
})(this, function() {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 25);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

exports.isUndefined = isUndefined;
var isObject = exports.isObject = function isObject(obj) {
    return Object.prototype.toString.call(obj) === '[object Object]';
};

var isEmpty = exports.isEmpty = function isEmpty(obj) {
    if (!obj) {
        return true;
    }
    for (var key in obj) {
        return false;
    }
    return true;
};

var isElement = exports.isElement = function isElement(value) {
    return (typeof HTMLElement === 'undefined' ? 'undefined' : _typeof(HTMLElement)) === 'object' ? value instanceof HTMLElement : //DOM2
    value && (typeof value === 'undefined' ? 'undefined' : _typeof(value)) === "object" && value !== null && value.nodeType === 1 && typeof value.nodeName === "string";
};

function isUndefined(value) {
    return value === undefined;
}

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
// import VConsole from 'vconsole';
// var vConsole = new VConsole();


//抛出错误警告
var warnError = exports.warnError = function warnError(error, fn) {
    console.error(error);
    if (fn) {
        fn();
    }
};

/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});


//查询节点
var queryNode = exports.queryNode = function queryNode(el) {
	var elm = document.querySelector(el);
	return elm;
};

//创建元素节点
var createElement = exports.createElement = function createElement(el) {
	return document.createElement(el);
};

//创建文本节点
var createTextNode = exports.createTextNode = function createTextNode(text) {
	return document.createTextNode(text);
};

//替换节点
var repalceNode = exports.repalceNode = function repalceNode(el, newNode, oldNode) {
	el.replaceChild(newNode, oldNode);
};

//设置节点文本内容
var setText = exports.setText = function setText(el, text) {
	return el.textContent = text;
};

//设置节点属性
var setAttribute = exports.setAttribute = function setAttribute(el, attrKey, attrValue) {
	el.setAttribute(attrKey, attrValue);
};

//设置节点值
var setElementValue = exports.setElementValue = function setElementValue(el, valueKey, value) {
	if (!el[valueKey]) {
		return false;
	}
	el[valueKey] = value;
	return true;
};

//复制节点
var cloneElement = exports.cloneElement = function cloneElement(el, flag) {
	return el.cloneElement(flag ? true : false);
};

//删除属性
var removeAttribute = exports.removeAttribute = function removeAttribute(el, attrKey) {
	el.removeAttribute(attrKey);
};

//删除子节点
var removeChild = exports.removeChild = function removeChild(el, child) {
	el.removeChild(child);
};

//获取父节点
var getParent = exports.getParent = function getParent(el) {
	return el.parentNode ? el.parentNode : false;
};

//加入子节点
var appendChild = exports.appendChild = function appendChild(el, child) {
	el.appendChild(child);
};

//插入新节点
var insertBefore = exports.insertBefore = function insertBefore(el, newChild, child) {
	el.insertBefore(newChild, child);
};

/***/ }),
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.createListVNode = exports.createEmptyVnode = exports.createVNodeText = exports.createVNodeElement = undefined;

var _util = __webpack_require__(0);

var _error = __webpack_require__(1);

//虚拟节点
var Vnode = function Vnode(option) {
    this.VnodeType = null;
    this.tagName = null;
    this.elm = null;
    this.attrs = null;
    this.children = null;
    this.events = null;
    this.directives = null;
    this.parent = null;
    this.text = null;
    this.empty = null;

    return this;
};

//构建虚拟节点
var createVNodeElement = exports.createVNodeElement = function createVNodeElement(tag, attrs, children, events, directives, isRoot) {
    var el = new Vnode();
    el.VnodeType = 1;
    el.tagName = tag;
    el.attrs = attrs;
    el.children = children;
    el.events = events;
    el.directives = directives;
    // el.beforeNode = null;
    // el.nextNode = null

    //根组件这里做下特殊处理 给子组件遍历一次 绑定父组件关系
    //这里可能存在性能损耗
    if (isRoot) {
        //这里有问题 需要改
        el.parent = isRoot;
        el.isRoot = true;
        if (Array.isArray(el.children) && el.children.length > 0) {
            childrenBindParent(el.children, el);
        }
    }

    //为子节点绑定父节点关系
    function childrenBindParent(children, parent) {
        if (!Array.isArray(children) || children.length === 0) {
            return false;
        }
        for (var i = 0; i < children.length; i++) {
            var node = children[i];
            //vm-for 接收的数据特殊处理 这里还不优雅 需要进一步处理
            if (Array.isArray(node)) {
                node.map(function (item, index) {
                    index = i + 1 + index;
                    item.parent = parent;
                    children.splice(index, 0, item);
                });
                children.splice(i, 1);
                continue;
            }
            //建立父节点关系
            node.parent = parent;
            if (Array.isArray(node.children) && node.children.length > 0) {
                childrenBindParent(node.children, node);
            }
        }
    }
    return el;
};

//构建虚拟文本节点
var createVNodeText = exports.createVNodeText = function createVNodeText(text) {
    var el = new Vnode();
    el.VnodeType = 2;
    el.text = text;
    return el;
};

//构建空的虚拟节点
var createEmptyVnode = exports.createEmptyVnode = function createEmptyVnode() {
    var el = new Vnode();
    el.VnodeType = 1;
    el.empty = true;
    return el;
};

//创建列表虚拟节点
var createListVNode = exports.createListVNode = function createListVNode(source, generatefn) {
    if (!Array.isArray(source) && !(0, _util.isObject)(source)) {
        (0, _error.warnError)('vm-for error: ' + source + ' is not array or object ');
    }
    var VNode = [];
    if (Array.isArray(source)) {
        VNode = source.map(function (item, index) {
            return generatefn(item, index);
        });
    } else {
        for (var key in source) {
            VNode.push(generatefn(source[key]));
        }
    }
    return VNode;
};

exports.default = Vnode;

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});
exports.cerateElementBindAddParent = exports.cerateElement = undefined;

var _domOperation = __webpack_require__(2);

var nodeOp = _interopRequireWildcard(_domOperation);

var _util = __webpack_require__(0);

var _process = __webpack_require__(5);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

//创建组件
var cerateElement = function cerateElement(Vnode) {
	var tagName, children;

	if (Vnode.VnodeType === 1 && !Vnode.empty) {
		tagName = Vnode.tagName;
		children = Vnode.children;

		Vnode.elm = nodeOp.createElement(tagName);
		//处理指令
		(0, _process.processDirective)(Vnode, Vnode.directives);
		//设置属性
		(0, _process.setAttribute)(Vnode.elm, Vnode.attrs);
		//设置事件
		(0, _process.setEventListener)(Vnode.elm, Vnode.events);

		//递归更新生成子元素
		if (Array.isArray(children) && children.length > 0) {
			for (var i = 0; i < children.length; i++) {
				cerateElementBindAddParent(children[i], Vnode.elm);
			}
		}
	} else if (Vnode.VnodeType === 2 && !Vnode.empty) {
		Vnode.elm = nodeOp.createTextNode(Vnode.text);
	}
	return Vnode;
};

//创建组件并且添加到父节点中
var cerateElementBindAddParent = function cerateElementBindAddParent(Vnode, parentElm) {
	var Vnode = cerateElement(Vnode);
	nodeOp.appendChild(parentElm, Vnode.elm);
};

exports.cerateElement = cerateElement;
exports.cerateElementBindAddParent = cerateElementBindAddParent;

/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.processShowDirective = exports.processClassDirective = exports.processModelDirective = exports.processDirective = exports.setEventListener = exports.setAttribute = undefined;

var _domOperation = __webpack_require__(2);

var nodeOp = _interopRequireWildcard(_domOperation);

var _util = __webpack_require__(0);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

// style="xxxx=xxx(;)"判断正则
var styleSeparatorReg = /(;)+\s*$/;

//设置属性
var setAttribute = function setAttribute(refElm, attrs) {
    for (var key in attrs) {
        //checked 特殊处理
        if (key === 'checked') {
            //因为这里是转换成了字符串
            if (JSON.parse(attrs[key])) {
                nodeOp.setAttribute(refElm, key, 'checked');
            }
            continue;
        }
        nodeOp.setAttribute(refElm, key, attrs[key]);
    }
};

//绑定事件
var setEventListener = function setEventListener(refElm, events) {
    if (!Array.isArray(events) || events.length === 0) {
        return false;
    }
    for (var i = 0; i < events.length; i++) {
        (function (count) {
            //这个标志用来解决闭包呢concat el问题
            var flag = true;
            events[count].instance = function (el) {
                if (!events[count].params) {
                    events[count].params = [];
                }
                //组装参数
                if (flag) {
                    events[count].params = events[count].params.concat([el]);
                    flag = false;
                }
                //执行
                events[count].exp.apply(this, events[count].params);
            };
        })(i);
        refElm.addEventListener(events[i].name, events[i].instance);
    }
};

//设置相应指令
//对应的删除还没做
var processDirective = function processDirective(Vnode, directives) {
    if (!Array.isArray(directives) || directives.length === 0) {
        return false;
    }
    for (var i = 0; i < directives.length; i++) {
        var directive = directives[i];

        //model特殊处理
        if (directive.name === 'model' && !directive.event) {
            processModelDirective(Vnode, directive);
            continue;
        }
        //class特殊处理
        if (directive.name === 'class' && !(0, _util.isEmpty)(directive.exp)) {
            processClassDirective(Vnode, directive);
            continue;
        }
        //show特殊处理
        if (directive.name === 'show') {
            processShowDirective(Vnode, directive);
            continue;
        }
    }
};

//处理model指令
var processModelDirective = function processModelDirective(Vnode, directive) {
    directive.event = function (el) {
        directive.exp(el.target.value);
    };
    Vnode.attrs['value'] = directive.value;
    Vnode.elm.addEventListener('input', directive.event);
};

//处理class指令
var processClassDirective = function processClassDirective(Vnode, directive) {
    var attrs = Vnode.attrs;
    var classList = '';

    //属性中是否存在class
    if (!(0, _util.isEmpty)(attrs)) {
        classList = attrs['class'] != null ? attrs['class'] : '';
    }
    //遍历class exp
    for (var className in directive.exp) {
        if (directive.exp[className]) {
            classList += ' ' + className;
        }
    }
    if (classList.length > 0) {
        Vnode.attrs['class'] = classList;
    }
};

//处理show指令
var processShowDirective = function processShowDirective(Vnode, directive) {
    var style = Vnode.attrs['style'];
    if (style && style.length > 0) {
        var display = directive.exp ? 'display:show;' : 'display:none;';
        style += styleSeparatorReg.test(style) ? display : ';' + display;
    } else {
        style = directive.exp ? 'display:show;' : 'display:none;';
    }
    Vnode.attrs['style'] = style;
};

exports.setAttribute = setAttribute;
exports.setEventListener = setEventListener;
exports.processDirective = processDirective;
exports.processModelDirective = processModelDirective;
exports.processClassDirective = processClassDirective;
exports.processShowDirective = processShowDirective;

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});


var expReg = /\{\{((?:.|\n)+?)\}\}/;

// filter还没做
var parseText = exports.parseText = function parseText(text) {
    var tokenExp = [];
    var expMatch;
    var nextIndex,
        lastIndex = 0;
    if (!expReg.test(text)) {
        return false;
    }

    while (expMatch = expReg.exec(text)) {
        //如果表达式之前有内容
        if (expMatch.index > lastIndex) {
            tokenExp.push('"' + text.slice(0, expMatch.index) + '"');
            text = text.substring(expMatch.index);
        }
        //中间表达式内容
        lastIndex = expMatch.index;
        text = text.substring(expMatch[0].length);
        tokenExp.push('_s(' + expMatch[1].trim() + ')');
        //判断中间是否还有字符
        nextIndex = text.indexOf('{{');
        if (nextIndex > 0) {
            tokenExp.push('"' + text.slice(0, nextIndex) + '"');
            text = text.substring(nextIndex);
        }
        //表达式之后还有字符
        if (lastIndex !== 0) {
            //如果后面还有表达式
            if (expReg.test(text)) {
                continue;
            } else {
                tokenExp.push('"' + text.slice(0, lastIndex) + '"');
            }
        }
    }
    if (lastIndex < text.length) {
        tokenExp.push('"' + text.slice(lastIndex, text.length) + '"');
    }

    return tokenExp.join('+');
};

/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});


var did = 0;

//辅助绑定对象
var Dep = function Dep() {
	this.id = did++;
	this.sub = [];
};
//全局对象
Dep.target = null;
//添加watcher
Dep.prototype.add = function (watcher) {
	this.sub.push(watcher);
};
//通知watcher更新
Dep.prototype.notify = function () {
	for (var i = 0; i < this.sub.length; i++) {
		this.sub[i].update();
	}
};

var setDepTarget = exports.setDepTarget = function setDepTarget(target) {
	Dep.target = target;
};

var clearDepTarget = exports.clearDepTarget = function clearDepTarget() {
	Dep.target = null;
};

exports.default = Dep;

/***/ }),
/* 8 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.defineEventProxy = exports.defineDataProxy = undefined;

var _util = __webpack_require__(0);

//当前目前属性映射到scopeKey
function setProxy(target, scopeKey, key) {
    var objDefineProperty = {
        configurable: false,
        enumerable: true,
        set: function set(value) {
            target[scopeKey][key] = value;
        },
        get: function get() {
            return target[scopeKey][key];
        }
    };
    Object.defineProperty(target, key, objDefineProperty);
}

//设置代理
var defineDataProxy = exports.defineDataProxy = function defineDataProxy(vue) {
    //当前目前属性映射到_data
    for (var key in vue._data) {
        if ((0, _util.isUndefined)(vue[key])) {
            setProxy(vue, '_data', key);
        }
    }
};

//设置事件代理
var defineEventProxy = exports.defineEventProxy = function defineEventProxy(vue) {
    //当前目前属性映射到_method
    for (var key in vue._method) {
        if ((0, _util.isUndefined)(vue[key])) {
            setProxy(vue, '_method', key);
        }
    }
};

/***/ }),
/* 9 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _error = __webpack_require__(1);

Object.keys(_error).forEach(function (key) {
  if (key === "default" || key === "__esModule") return;
  Object.defineProperty(exports, key, {
    enumerable: true,
    get: function get() {
      return _error[key];
    }
  });
});

var _tool = __webpack_require__(10);

Object.keys(_tool).forEach(function (key) {
  if (key === "default" || key === "__esModule") return;
  Object.defineProperty(exports, key, {
    enumerable: true,
    get: function get() {
      return _tool[key];
    }
  });
});

/***/ }),
/* 10 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.bind = exports.toStringify = exports.makeMap = undefined;

var _error = __webpack_require__(1);

//设置映射
var makeMap = exports.makeMap = function makeMap(key, valueString) {
    var map = {};
    var list = typeof valueString === 'string' ? valueString.split(',') : valueString.toString().split(',');
    if (Array.isArray(list) && list.length > 0) {
        map[key] = list;
    } else {
        (0, _error.warnError)('set map error: valueString type error or value error');
        return false;
    }
    return function (value) {
        return map[key].indexOf(value) !== -1 ? true : false;
    };
};

//构建转译字符串
var toStringify = exports.toStringify = function toStringify(text) {
    if (typeof text === 'string') {
        return text;
    }
    return text.toString();
};

//方法中绑定当前作用域
var bind = exports.bind = function bind(event) {
    var self = this;
    return function () {
        event.apply(self, arguments);
    };
};

/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});

var _index = __webpack_require__(9);

var _render = __webpack_require__(21);

var _data = __webpack_require__(19);

var _method = __webpack_require__(20);

var _watcher = __webpack_require__(23);

var _watcher2 = _interopRequireDefault(_watcher);

var _index2 = __webpack_require__(3);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var uid = 0;

var VueMini = function VueMini(option) {
    if (!option) {
        (0, _index.warnError)('Init error :option not undefined');
        return this;
    }
    this.id = uid++;
    this.self = this;
    this._el = option.el;
    this._option = option;
    this._rootParent = option.rootParent ? option.rootParent : '';

    //生命周期 后面还需完善
    if (option.updated && typeof option.updated === 'function') {
        this.updated = option.updated;
    }

    if (option.mounted && typeof option.mounted === 'function') {
        this.mounted = option.mounted;
    }

    //执行初始化
    this.$init();
};

/**************  原型方法   ***************/
//初始化
VueMini.prototype.$init = function () {
    //初始化编译	
    (0, _render.initCompiler)(this);
    //初始化data数据建立绑定
    (0, _data.initData)(this);
    //初始化mothod方法
    (0, _method.initMethod)(this);

    //开始挂载
    this.$mount();

    return this;
};

//挂载
VueMini.prototype.$mount = function () {
    if (!this._render) {
        (0, _index.warnError)('$mount error: VueMini._render  is not defined');
    }
    var $render = this._render;
    this._watcher = new _watcher2.default(this, $render);
};

/**********   render中使用  ***********/
VueMini.prototype._s = _index.toStringify;
VueMini.prototype._c = _index2.createVNodeElement;
VueMini.prototype._v = _index2.createVNodeText;
VueMini.prototype._l = _index2.createListVNode;
VueMini.prototype._e = _index.bind;

exports.default = VueMini;

/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.patch = undefined;

var _create = __webpack_require__(4);

var _update = __webpack_require__(13);

//将虚拟节点更新到真实DOM中
var patch = exports.patch = function patch(oldNode, Vnode, isRoot) {
    //是否是根节点 并且老节点是空节点
    if (isRoot && oldNode.empty) {
        //创建新元素 更新到DOM
        (0, _create.cerateElementBindAddParent)(Vnode, Vnode.parent);
        //删除老节点 从DOM中删除对应真实节点
        (0, _update.removeElement)(oldNode);
    } else if (oldNode && Vnode) {
        //不是的话 那么更新对比节点
        (0, _update.updateElement)(oldNode, Vnode);
    }
    return Vnode;
};

/***/ }),
/* 13 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.updateChildren = exports.updateText = exports.updateInput = exports.updateAttrs = exports.updateDirective = exports.updateElement = exports.removeElement = undefined;

var _domOperation = __webpack_require__(2);

var nodeOp = _interopRequireWildcard(_domOperation);

var _util = __webpack_require__(0);

var _create = __webpack_require__(4);

var _process = __webpack_require__(5);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

/**************************    删除部分         *****************************/

//这里还不完整
var removeElement = function removeElement(Vnode) {
    //如果是body 不能删除
    if ((0, _util.isElement)(Vnode.elm) && Vnode.elm.nodeName === 'BODY') {
        return false;
    }
    //正常卸载
    if (Vnode.empty) {
        var parent = nodeOp.getParent(Vnode.elm);
        nodeOp.removeChild(parent, Vnode.elm);
    } else {
        //这里还要卸载相应事件等等
        //没有卸载相关事件,可能导致内存溢出
        //暂未完成
        var parent = nodeOp.getParent(Vnode.elm);
        nodeOp.removeChild(parent, Vnode.elm);
    }
};

/**************************    更新部分         *****************************/
//这里还不完整
var updateElement = function updateElement(oldNode, Vnode) {
    //如果老节点存在,新节点不存在
    if (oldNode && !Vnode) {
        removeElement(oldNode);
        return false;
    }
    //如果新节点存在,老节点不存在
    if (!oldNode && Vnode) {
        var parentElm = Vnode.parent.elm;
        (0, _create.cerateElementBindAddParent)(Vnode, parentElm);
        return false;
    }
    //如果节点类型出现不匹配
    if (Vnode.VnodeType !== oldNode.VnodeType) {
        //新老节点出现类型变化 则重新生产并且替换
        Vnode = (0, _create.cerateElement)(Vnode);
        nodeOp.repalceNode(Vnode.parent.elm, Vnode.elm, oldNode.elm);
        return false;
    }

    //更新组件
    if (Vnode.VnodeType === 1) {
        Vnode.elm = oldNode.elm;
        updateDirective(oldNode, Vnode);
        updateAttrs(oldNode, Vnode);
        updateChildren(oldNode.children, Vnode.children);
    } else if (Vnode.VnodeType === 2) {
        Vnode.elm = oldNode.elm;
        updateText(oldNode, Vnode);
    }
};

//更新指令
var updateDirective = function updateDirective(oldNode, Vnode) {
    var oldDirective = oldNode.directives;
    var nowDirective = Vnode.directives;
    if (!nowDirective || nowDirective.length === 0) {
        return false;
    }
    //遍历新指令依次做对比更新
    for (var i = 0; i < nowDirective.length; i++) {
        var now = nowDirective[i];
        var old = getOldDirectiveValue(now.name);
        //处理相应指令
        switch (now.name) {
            case 'model':
                if (!old) {
                    (0, _process.processModelDirective)(Vnode, now);
                } else if (old.value !== now.value) {
                    //更新值
                    Vnode.attrs['value'] = now.value;
                } else {
                    Vnode.attrs['value'] = old.value;
                }
                break;
            case 'class':
                //这里可能存在性能损耗
                (0, _process.processClassDirective)(Vnode, now);
                break;
            case 'show':
                //这里可能存在性能损耗
                (0, _process.processShowDirective)(Vnode, now);
                break;
        }
    }

    //获取旧指令集合中指令名对应的指令
    function getOldDirectiveValue(directiveName) {
        if (!oldDirective || oldDirective.length === 0) {
            return false;
        }
        for (var i = 0; i < oldDirective.length; i++) {
            if (oldDirective[i].name === directiveName) {
                return oldDirective[i];
            }
        }
        return false;
    }
};

//更新属性
var updateAttrs = function updateAttrs(oldNode, Vnode) {
    var refElm = Vnode.elm;
    var oldAttrs = oldNode.attrs;
    var nowAttrs = Vnode.attrs;
    if (!oldAttrs && !nowAttrs) {
        return false;
    }
    //如果是input 特殊更新
    if (Vnode.tagName === 'input') {
        return updateInput(oldNode, Vnode);
    }
    var nowKeys = nowAttrs && !(0, _util.isEmpty)(nowAttrs) ? Object.keys(nowAttrs) : [];
    //遍历新属性集合更新对应节点属性
    for (var i = 0; i < nowKeys.length; i++) {
        var key = nowKeys[i];
        //如果老节点没有
        if (!oldAttrs[key]) {
            //checked 特殊处理
            nodeOp.setAttribute(refElm, key, nowAttrs[key]);
            continue;
        }
        //如果老节点有 那么比较
        if (oldAttrs[key] !== nowAttrs[key]) {
            nodeOp.setAttribute(refElm, key, nowAttrs[key]);
            delete oldAttrs[key];
        } else {
            //相同则删除老节点的
            delete oldAttrs[key];
        }
    }
    //遍历旧属性集合 这里剩下的都是需要删除的
    var oldKeys = oldAttrs && !(0, _util.isEmpty)(oldAttrs) ? Object.keys(oldAttrs) : [];
    for (var i = 0; i < oldKeys.length; i++) {
        var key = oldKeys[i];
        nodeOp.removeAttribute(refElm, key);
        delete oldAttrs[key];
    }
};

//更新文本内容
var updateText = function updateText(oldNode, Vnode) {
    if (oldNode.text !== Vnode.text) {
        nodeOp.setText(Vnode.elm, Vnode.text);
    }
};

//更新子节点
var updateChildren = function updateChildren(oldChildren, children) {
    //子节点递归更新
    var len = children.length;
    for (var i = 0; i < len; i++) {
        var Vnode = children[i];
        var oldVnode;

        if (oldChildren && oldChildren[i]) {
            oldVnode = oldChildren[i];
        } else {
            oldVnode = false;
        }
        updateElement(oldVnode, Vnode);
    }

    if (oldChildren) {
        //删除余下的老节点
        for (var i = len; i < oldChildren.length; i++) {
            updateElement(oldChildren[i], false);
        }
    }
};

//input textare 等 value问题特殊更新
var updateInput = function updateInput(oldNode, Vnode) {
    var refElm = Vnode.elm;
    var oldAttrs = oldNode.attrs;
    var nowAttrs = Vnode.attrs;
    if (!oldAttrs && !nowAttrs) {
        return false;
    }
    var nowKeys = nowAttrs && !(0, _util.isEmpty)(nowAttrs) ? Object.keys(nowAttrs) : [];
    //遍历新属性集合更新对应节点属性
    for (var i = 0; i < nowKeys.length; i++) {
        var key = nowKeys[i];
        //如果老节点没有
        if (!oldAttrs[key]) {
            switch (key) {
                case 'checked':
                    if (JSON.parse(nowAttrs[key])) {
                        nodeOp.setAttribute(refElm, key, 'checked');
                    }
                    break;
                case 'value':
                    nodeOp.setElementValue(refElm, key, nowAttrs[key]);
                    break;
                default:
                    nodeOp.setAttribute(refElm, key, nowAttrs[key]);
            }
            continue;
        }
        //如果老节点有 那么比较
        if (oldAttrs[key] !== nowAttrs[key]) {
            switch (key) {
                case 'checked':
                    if (JSON.parse(nowAttrs[key])) {
                        nodeOp.setAttribute(refElm, key, 'checked');
                    } else {
                        nodeOp.removeAttribute(refElm, key);
                    }
                    break;
                case 'value':
                    nodeOp.setElementValue(refElm, key, nowAttrs[key]);
                    break;
                default:
                    nodeOp.setAttribute(refElm, key, nowAttrs[key]);
            }
            delete oldAttrs[key];
        } else {
            //相同则删除老节点的
            delete oldAttrs[key];
        }
    }
    //遍历旧属性集合 这里剩下的都是需要删除的
    var oldKeys = oldAttrs && !(0, _util.isEmpty)(oldAttrs) ? Object.keys(oldAttrs) : [];
    for (var i = 0; i < oldKeys.length; i++) {
        var key = oldKeys[i];
        nodeOp.removeAttribute(refElm, key);
        delete oldAttrs[key];
    }
};

exports.removeElement = removeElement;
exports.updateElement = updateElement;
exports.updateDirective = updateDirective;
exports.updateAttrs = updateAttrs;
exports.updateInput = updateInput;
exports.updateText = updateText;
exports.updateChildren = updateChildren;

/***/ }),
/* 14 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.generateCode = undefined;

var _textParse = __webpack_require__(6);

//编译生成可执行code
var generateCode = exports.generateCode = function generateCode(AST) {
    var tag = AST.tagName;
    var attrs = genreateAttr(AST.attrs);
    var children = generateChildren(AST.children);
    var EventCode = generateEvent(AST.event);
    var directiveCode = generateDirective(AST.directive);

    return 'with(this){return _c("' + tag + '",' + attrs + ',' + children + ',' + EventCode + ',' + directiveCode + ',_rootParent)}';
};

//vm-for directive特殊生成
function generateForCode(node) {
    var tag = node.tagName;
    var attrs = genreateAttr(node.attrs);
    var children = generateChildren(node.children);
    var EventCode = generateEvent(node.event);
    var directiveCode = generateDirective(node.directive);
    return '_l(' + node.forSource + ',function(' + node.forkey + ',$index){ return _c("' + tag + '",' + attrs + ',' + children + ',' + EventCode + ',' + directiveCode + ')})';
}

//vm-if directive特殊生成
function generateIFCode(node) {
    var tag = node.tagName;
    var type = node.type;
    var ifExp = node.ifExp;
    switch (type) {
        case 1:
            var attrs = genreateAttr(node.attrs);
            var children = generateChildren(node.children);
            var EventCode = generateEvent(node.event);
            var directiveCode = generateDirective(node.directive);
            return '(' + ifExp + ')? _c("' + tag + '",' + attrs + ',' + children + ',' + EventCode + ',' + directiveCode + '): _v("")';
        case 2:
            return '(' + ifExp + ')? _v(_s(' + node.exp + ')) : _v("")';
        case 3:
            return '(' + ifExp + ')? _v("' + node.text + '") : _v("")';
    }
    return '_v("")';
}

//生成子节点Code函数
function generateChildren(children) {
    if (!Array.isArray(children) || children.length === 0) {
        return '[]';
    }
    var CodeList = [];
    for (var i = 0; i < children.length; i++) {
        var node = children[i];
        //如果是vm-for命令特殊处理
        if (node.isFor) {
            CodeList.push(generateForCode(node));
            continue;
        }
        //如果是vm-if命令特殊处理
        if (node.isIf) {
            CodeList.push(generateIFCode(node));
            continue;
        }
        //普通子节点处理
        switch (node.type) {
            case 1:
                //解析生成事件
                var EventCode = generateEvent(node.event);
                //解析指令
                var directiveCode = generateDirective(node.directive);
                //递归绑定子节点
                if (Array.isArray(node.children) || node.children.length > 0) {
                    CodeList.push('_c("' + node.tagName + '",' + genreateAttr(node.attrs) + ',' + generateChildren(node.children) + ',' + EventCode + ',' + directiveCode + ')');
                } else {
                    CodeList.push('_c("' + node.tagName + '",' + genreateAttr(node.attrs) + ',[],' + EventCode + ',' + directiveCode + ')');
                }
                break;
            case 2:
                CodeList.push('_v(_s(' + node.exp + '))');
                break;
            case 3:
                CodeList.push('_v("' + node.text + '")');
                break;
        }
    }
    return '[' + CodeList.join(',') + ']';
}

//生成事件Code函数
function generateEvent(events) {
    if (!Array.isArray(events) || events.length === 0) {
        return '[]';
    }

    var EventCodeList = [];

    for (var i = 0; i < events.length; i++) {
        var eve = events[i];
        if (eve.params) {
            EventCodeList.push('{name:"' + eve.name + '",exp:_e(' + eve.exp + '),params:[' + eve.params + ']}');
        } else {
            EventCodeList.push('{name:"' + eve.name + '",exp:_e(' + eve.exp + ')}');
        }
    }

    return '[' + EventCodeList.join(',') + ']';
}

//生成指令Code函数
function generateDirective(directives) {
    if (!Array.isArray(directives) || directives.length === 0) {
        return '[]';
    }

    var directiveCode = [];
    for (var i = 0; i < directives.length; i++) {
        var direc = directives[i];
        if (direc.name === 'model') {
            directiveCode.push('{name:"' + direc.name + '",exp:function(val){ ' + direc.exp + ' = val },value:' + direc.exp + '}');
            continue;
        }
        if (direc.name === 'class') {
            directiveCode.push('{name:"' + direc.name + '",exp:' + direc.exp + '}');
            continue;
        }
        if (direc.name === 'show') {
            directiveCode.push('{name:"' + direc.name + '",exp:' + direc.exp + '}');
            continue;
        }
    }

    return '[' + directiveCode.join(',') + ']';
}

//生成attributeCode函数
function genreateAttr(attrs) {
    var expReg = /\{\{((?:.|\n)+?)\}\}/;
    var attrsCode = '{';
    if (!Array.isArray(attrs) || attrs.length === 0) {
        return '{}';
    }
    for (var i = 0; i < attrs.length; i++) {
        //判断表达式中间是否有解析式
        var exp = (0, _textParse.parseText)(attrs[i].value);
        if (!exp) {
            attrsCode += '"' + attrs[i].name + '":';
            attrsCode += '"' + attrs[i].value + '"';
        } else {
            attrsCode += '"' + attrs[i].name + '":';
            attrsCode += exp;
        }
        if (i + 1 < attrs.length) {
            attrsCode += ',';
        }
    }
    attrsCode += '}';
    return attrsCode;
}

/***/ }),
/* 15 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.compileTemplateToFn = undefined;

var _index = __webpack_require__(18);

var _generate = __webpack_require__(14);

//编译模板字符串为可执行函数
var compileTemplateToFn = exports.compileTemplateToFn = function compileTemplateToFn(template, options, context) {
    //开始解析模板 生成虚拟元素
    var ast = (0, _index.parse)(template.trim(), options);
    // console.log('AST',ast)

    var render = (0, _generate.generateCode)(ast, options.rootEl);
    // console.log('render', render)
    // debugger;

    return new Function(render);
};

/***/ }),
/* 16 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.parseAttrs = parseAttrs;

var _error = __webpack_require__(1);

//捕获事件
var eventReg = /vm-on([a-zA-Z]+)/;
//捕获事件是否穿参数
var eventParamsReg = /^([a-zA-Z,"']+)\(([$\w,"']+)\)/;
//捕获指令
var directiveReg = /vm-([a-z]+)/;
//捕获vnm-for内容
var forReg = /^\s*([\w]+)\s*in\s*([\w]+)/;

//设置attribute映射
function setAttributeMap(attrs) {
    var attribute = {};
    for (var i = 0; i < attrs.length; i++) {
        attribute[attrs[i].name] = attrs[i].value;
    }
    return attribute;
}

//提取attribute
function getAttributeMap(attrMap, attrKey) {
    var keys = Object.keys(attrMap);
    if (keys.indexOf(attrKey) !== -1) {
        var attrValue = attrMap[attrKey];
        delete attrMap[attrKey];
        return attrValue;
    }
}

//编译节点属性
function parseAttrs(astElm, attrs) {
    var attrsMap = setAttributeMap(attrs);
    var elm = astElm;
    for (var i = 0; i < attrs.length; i++) {
        var key = attrs[i].name;
        //处理指令
        processDirective(elm, key, attrsMap);
        //处理事件
        processEvent(elm, key, attrsMap);
    }
    //特殊处理vm-if
    processIf(elm);
    //特殊处理vm-for
    processFor(elm);
    //处理剩余的attr
    processSurplus(elm, attrsMap);
}

//处理vm-xxx等指令
function processDirective(elm, attrKey, attrMap) {
    if (!directiveReg.test(attrKey) || /vm-on/.test(attrKey)) {
        return false;
    }
    var directive = {};
    directive.name = attrKey.match(directiveReg)[1];
    directive.exp = getAttributeMap(attrMap, attrKey);
    elm.directive.push(directive);
}

//处理vm-if指令
function processIf(elm) {
    if (!Array.isArray(elm.directive) || elm.directive.length === 0) {
        return false;
    }
    var directive;
    for (var i = 0; i < elm.directive.length; i++) {
        if (elm.directive[i].name === 'if') {
            directive = elm.directive[i];
            break;
        }
    }
    if (!directive) {
        return false;
    }
    elm.isIf = true;
    elm.ifExp = directive.exp;
    elm.directive.splice(i, 1);
}

//处理vm-for指令
function processFor(elm) {
    if (!Array.isArray(elm.directive) || elm.directive.length === 0) {
        return false;
    }
    var directive;
    for (var i = 0; i < elm.directive.length; i++) {
        if (elm.directive[i].name === 'for') {
            directive = elm.directive[i];
            break;
        }
    }
    if (!directive) {
        return false;
    }
    elm.isFor = true;
    var forMatch = directive.exp.match(forReg);
    if (forMatch && forMatch[1] && forMatch[2]) {
        elm.forkey = forMatch[1];
        elm.forSource = forMatch[2];
    } else {
        (0, _error.warnError)('compiler error: directive vm-for parse is error');
        elm.isFor = false;
        return false;
    }
}

//处理vm-XXXX事件
//未完成
function processEvent(elm, attrKey, attrMap) {
    if (!eventReg.test(attrKey)) {
        return false;
    }
    var event = {};
    event.name = attrKey.match(eventReg)[1].toLowerCase();
    event.exp = getAttributeMap(attrMap, attrKey);

    //如果存在包裹参数
    if (eventParamsReg.test(event.exp)) {
        var exp = event.exp.match(eventParamsReg)[1];
        var params = event.exp.match(eventParamsReg)[2];
        //判断是否传入多个参数
        if (params.indexOf(',') > -1) {
            params = params.split(',');
        }
        event.params = params;
        event.exp = exp;
    }
    elm.event.push(event);
}

//处理剩余attr
function processSurplus(elm, attrMap) {
    for (var key in attrMap) {
        var attr = {};
        attr.name = key;
        attr.value = getAttributeMap(attrMap, key);
        elm.attrs.push(attr);
    }
}

/***/ }),
/* 17 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});


//捕获attribute的正则
// key = /^\s*([^\s"'<>\/=]+)/
// assing = /?:\s*((?:=))/
// value = /\s*(?:"([^"]*)"+|'([^']*)'+|([^\s"'=<>`]+))/
var attributeReg = /^\s*([^\s"'<>\/=]+)(?:\s*((?:=))\s*(?:"([^"]*)"+|'([^']*)'+|([^\s"'=<>`]+)))?/;

//标签名
var ncname = '[a-zA-Z_][\\w\\-\\.]*';
var qnameCapture = '((?:' + ncname + '\\:)?' + ncname + ')';

//开始标签开头
var startTagOpen = new RegExp('^<' + qnameCapture);
//开始标签结束
var startTagClose = /^\s*(\/?)>/;
//结束标签
var endTag = new RegExp('^<\\/' + qnameCapture + '[^>]*>');

//doctype
var doctype = /^<!DOCTYPE [^>]+>/i;
//comment
var comment = /^<!--/;
//conditional
var conditionalComment = /^<!\[/;

//为了解决某些版本 编译转化的问题
var decodingMap = {
	'&lt;': '<',
	'&gt;': '>',
	'&quot;': '"',
	'&amp;': '&',
	'&#10;': '\n'
	//不编译换行
};var encodedAttr = /&(?:lt|gt|quot|amp);/g;
//编译换行
var encodedAttrWithNewLines = /&(?:lt|gt|quot|amp|#10);/g;

//火狐正则bug
var IS_REGEX_CAPTURING_BROKEN = false;
'x'.replace(/x(.)?/g, function (m, g) {
	IS_REGEX_CAPTURING_BROKEN = g === '';
});

//解析html
var parseHTML = exports.parseHTML = function parseHTML(html, option) {
	var stack = [];
	//是否不可嵌套节点
	var canBeleftOpenTag = option.canBeleftOpenTag;
	//是否自闭和节点
	var isUnaryTag = option.isUnaryTag;
	//不可包裹节点
	var isNonPhrasingTag = option.isNonPhrasingTag;
	//是否是IE
	var isIE = option.isIE;
	//解析下标
	var index = 0;
	var last, lastTag;
	//解析开始
	while (html) {
		last = html;
		//找到标签开始
		var textEnd = html.indexOf('<');
		//开始标签
		if (textEnd === 0) {
			//判断是否是注释
			if (comment.test(html)) {
				var commentEnd = html.indexOf('-->');
				if (commentEnd >= 0) {
					advance(commentEnd + 3);
					continue;
				}
			}
			//判断条件标签
			if (conditionalComment.test(html)) {
				var conditionalEnd = html.indexOf(']>');
				if (conditionalEnd >= 0) {
					advance(conditionalEnd + 2);
					continue;
				}
			}
			//DOCTYPE
			var doctypeMatch = html.match(doctype);
			if (doctypeMatch) {
				advance(doctypeMatch[0].length);
				continue;
			}
			//结束标签
			var endTagMatch = html.match(endTag);
			if (endTagMatch) {
				var curIndex = index;
				advance(endTagMatch[0].length);
				parseEndTag(endTagMatch[1]);
				continue;
			}
			//开始标签
			var startTagMatch = parseStartTag();
			if (startTagMatch) {
				handleStartTag(startTagMatch);
				continue;
			}
		}

		//文本内容
		var text, rest, next;
		if (textEnd >= 0) {
			//文本内容
			rest = html.slice(textEnd);
			//推进到下阶段字符串
			while (!endTag.test(rest) && !startTagOpen.test(rest) && !comment.test(rest) && !conditionalComment.test(rest)) {
				next = rest.indexOf('<', 1);
				if (next < 0) break;
				textEnd += next;
				rest = html.slice(textEnd);
			}

			text = html.substring(0, textEnd);
			advance(textEnd);
		}

		if (textEnd < 0) {
			text = html;
			html = '';
		}

		if (option.chars && text) {
			option.chars(text);
		}
	}

	//向前推进
	function advance(n) {
		index += n;
		html = html.substring(n);
	}

	//解析开头标签
	function parseStartTag() {
		var tagMatch = html.match(startTagOpen);
		var end, attr;
		if (tagMatch) {
			var match = {
				tagName: tagMatch[1].toLowerCase(),
				attrs: [],
				start: index
			};
			advance(tagMatch[0].length);

			while (!(end = html.match(startTagClose)) && (attr = html.match(attributeReg))) {
				if (attr) {
					advance(attr[0].length);
					match.attrs.push(attr);
				}
			}

			if (end) {
				match.unarySlash = end[1];
				match.end = index;
				advance(end[0].length);
			}
			return match;
		}
	}

	//解析结束标签
	function parseEndTag(tagName) {
		var tagName = tagName.toLowerCase();
		var index = -1;
		for (var i = 0; i < stack.length; i++) {
			if (tagName = stack[i].tagName) {
				index = i;
				break;
			}
		}
		if (index >= 0) {
			//结束标签
			if (option.end) {
				option.end(stack[i].tagName);
			}
			//解析stack弹出
			lastTag = stack.pop();
		} else {
			throw '标签解析出错 ' + tagName + '未找到结束标签';
			return false;
		}
	}

	//处理标签内容
	function handleStartTag(match) {
		var tagName = match.tagName;
		var unarySlash = match.unarySlash;
		//P标签并且当前标签不可包裹
		if (lastTag === 'p' && isNonPhrasingTag(tagName)) {
			parseEndTag(lastTag);
		}
		//不可嵌套标签
		if (lastTag === tagName && canBeleftOpenTag(tagName)) {
			parseEndTag(tagName);
		}

		var isUnary = isUnaryTag(tagName) || !!unarySlash;

		var attrs = [];
		for (var i = 0; i < match.attrs.length; i++) {
			if (IS_REGEX_CAPTURING_BROKEN && match.attrs[i]) {
				if (!match.attrs[i][3]) {
					delete match.attrs[i][3];
				}
				if (!match.attrs[i][4]) {
					delete match.attrs[i][4];
				}
				if (!match.attrs[i][5]) {
					delete match.attrs[i][5];
				}
			}
			var value = match.attrs[i][3] || match.attrs[i][4] || match.attrs[i][5] || ' ';
			var re = encodedAttr;
			attrs.push({
				name: match.attrs[i][1],
				value: value.replace(re, function (match) {
					return decodingMap[match];
				})
			});
		}
		if (option.start) {
			stack.push(match);
			lastTag = tagName;
			option.start(tagName, attrs, isUnary);
		}
	}
};

/***/ }),
/* 18 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});
exports.parse = undefined;

var _element = __webpack_require__(24);

var _error = __webpack_require__(1);

var _htmlParse = __webpack_require__(17);

var _textParse = __webpack_require__(6);

var _attrParse = __webpack_require__(16);

var parse = exports.parse = function parse(template, options) {
	var stack = [];
	var root;
	var currentParent;

	//开始解析
	(0, _htmlParse.parseHTML)(template, {
		warnError: options.warnError,
		isNonPhrasingTag: _element.isNonPhrasingTag,
		canBeleftOpenTag: _element.canBeleftOpenTag,
		isUnaryTag: _element.isUnaryTag,
		isIE: false,
		// 处理开始标签
		// 处理attribute部分还没做
		start: function start(tag, attrs, unary) {
			//组件
			var element = {
				type: 1,
				tagName: tag,
				// isFor: false,
				// forkey: null,     vm-for专属数据
				// forSource: null
				// isIf: false,      vm-if标记
				// ifExp: null       vm-if表达式
				attrs: [],
				event: [],
				directive: [],
				children: []

				//编译处理提取到的attribute
			};(0, _attrParse.parseAttrs)(element, attrs);
			//如果根节点不存在
			if (!root) {
				if (element.isFor) {
					return (0, _error.warnError)('compiler error: rootElement  can`t  is vm-for directive');
				}
				root = element;
			}

			//父节点存在将自己加入父节点中
			if (currentParent) {
				currentParent.children.push(element);
				element.parent = currentParent;
			}
			//不是自闭合组件
			if (!unary) {
				//赋值自己当前父节点
				currentParent = element;
				//加入堆栈
				stack.push(element);
			}
		},
		//处理结束标签
		end: function end(tagName) {
			var el = stack.pop();
			var lastNode = el.children[el.children.length - 1];
			//移除最后一个空节点
			if (lastNode && lastNode.type === 3 && lastNode.text === '') {
				el.children.pop();
			}
			currentParent = stack[stack.length - 1];
		},
		//处理文本
		chars: function chars(text) {
			var expression;
			var text = text.trim();
			if (text !== ' ' && (expression = (0, _textParse.parseText)(text))) {
				currentParent.children.push({
					type: 2,
					exp: expression,
					text: null
				});
			} else {
				currentParent.children.push({
					type: 3,
					exp: null,
					text: text
				});
			}
		}
	});

	return root;
};

/***/ }),
/* 19 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.initData = undefined;

var _proxy = __webpack_require__(8);

var _index = __webpack_require__(22);

var initData = exports.initData = function initData(vue) {
    vue._data = vue._option.data;
    //绑定数据代理,将data数据赋值给自己

    (0, _proxy.defineDataProxy)(vue);
    //建立绑定拦截
    (0, _index.defineDataToObserver)(vue._data);
};

/***/ }),
/* 20 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});
exports.initMethod = undefined;

var _proxy = __webpack_require__(8);

var initMethod = exports.initMethod = function initMethod(vue) {
	vue._method = vue._option.method ? vue._option.method : false;
	if (vue._method) {
		//绑定数据代理,将method数据赋值给自己
		(0, _proxy.defineEventProxy)(vue);
	}
};

/***/ }),
/* 21 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});
exports.initCompiler = undefined;

var _error = __webpack_require__(1);

var _util = __webpack_require__(0);

var _index = __webpack_require__(15);

//查找元素 不存在则返回
function queryDom(el) {
	if (typeof el === 'string') {
		var dom = document.querySelector(el);
		return dom;
	} else if (el.nodeType && el.nodeType === 1) {
		return el;
	}
}

//获取标签全部内容
function getOuterHTML(el) {
	if (el.outerHTML) {
		return el.outerHTML;
	} else {
		var container = document.createElement('div');
		container.appendChild(el.cloneNode(true));
		return container.innerHtml;
	}
}

//初始化编译
var initCompiler = exports.initCompiler = function initCompiler(vue) {
	var template;
	var option = vue._option;
	var el = option.el;
	el = queryDom(el);
	if (!el || !(0, _util.isElement)(el)) {
		(0, _error.warnError)('Compiler Error: el not is Dom NodeType or not query element');
		return false;
	}
	if (!vue._rootParent && el.nodeName !== 'BODY') {
		vue._rootParent = el.parentNode;
	} else {
		vue._rootParent = el;
	}
	template = option.template || getOuterHTML(el);
	//编译元素
	var render = (0, _index.compileTemplateToFn)(template, {
		delimiters: option.delimiters,
		warnError: _error.warnError
	}, vue);
	vue._render = render;
};

/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.defineDataToObserver = undefined;

var _dep = __webpack_require__(7);

var _dep2 = _interopRequireDefault(_dep);

var _util = __webpack_require__(0);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var defineDataToObserver = exports.defineDataToObserver = function defineDataToObserver(data) {

    var keys = Object.keys(data);
    var dep = new _dep2.default();

    for (var i = 0; i < keys.length; i++) {
        var key = keys[i];
        if ((0, _util.isObject)(data[key]) || Array.isArray(data[key])) {
            //绑定这个数组或者对象
            defineObserver(data, key, data[key]);
            //递归绑定其中子元素
            defineDataToObserver(data[key]);
        } else {
            defineObserver(data, key, data[key]);
        }
    }

    //绑定观察者
    function defineObserver(target, key, value) {
        Object.defineProperty(target, key, {
            configurable: false,
            enumerable: true,
            set: function set(newVal) {
                if (value === newVal) {
                    return;
                }
                value = newVal;
                dep.notify();
            },
            get: function get() {
                if (_dep2.default.target) {
                    _dep2.default.target.add(dep);
                };
                return value;
            }
        });
    }
};

/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});

var _index = __webpack_require__(9);

var _util = __webpack_require__(0);

var _dep = __webpack_require__(7);

var _index2 = __webpack_require__(3);

var _patch = __webpack_require__(12);

var _domOperation = __webpack_require__(2);

var Watcher = function Watcher(vue, render) {
    this._vue = vue;
    this.depId = [];
    this._render = render;
    this.Vnode = null;
    //首次挂载
    this.render(true /*isRoot*/);
};

//添加绑定
Watcher.prototype.add = function (dep) {
    //如果没有重复的dep则建立绑定
    if (this.depId.indexOf(dep.id) === -1) {
        this.depId.push(dep.id);
        dep.add(this);
    }
};

//更新
Watcher.prototype.update = function () {
    var self = this;
    setTimeout(function () {
        self.render.call(self, false);
        //如果有更新回调事件,那么调用
        if (self._vue.updated) {
            self._vue.updated();
        }
    }, 0);
};

Watcher.prototype.render = function (isRoot) {
    var el = (0, _util.isElement)(this._vue._el) ? this._vue._el : this._vue._el = (0, _domOperation.queryNode)(this._vue._el);
    var Vnode;
    var oldVnode;
    if (!el) {
        (0, _index.warnError)('mount error: is new VueMini params el is no query dom');
        return false;
    }
    //生成
    try {
        (0, _dep.setDepTarget)(this);
        // var start  = new Date().getTime();
        Vnode = this._render.call(this._vue);
        (0, _dep.clearDepTarget)();
    } catch (e) {
        (0, _index.warnError)('mount error: is VueMini render error, detail message a ' + e);
        (0, _dep.clearDepTarget)();
    }
    //挂载
    try {
        if (this.Vnode) {
            oldVnode = this.Vnode;
        } else {
            oldVnode = (0, _index2.createEmptyVnode)();
            oldVnode.elm = el;
            //初次挂载事件
            if (this._vue.mounted) {
                var self = this;
                setTimeout(function () {
                    self._vue.mounted.call(self._vue);
                });
            }
        }
        //将虚拟节点 更新到真实dom上
        this.Vnode = (0, _patch.patch)(oldVnode, Vnode, isRoot /*isRoot*/);
        // var end = new Date().getTime();
        // console.log( end-start )
    } catch (e) {
        (0, _index.warnError)('mount error: is VueMini mount error, detail message a ' + e);
    }
};

exports.default = Watcher;

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
	value: true
});
exports.isUnaryTag = exports.canBeleftOpenTag = exports.isNonPhrasingTag = undefined;

var _tool = __webpack_require__(10);

//不能被分局的标签
var isNonPhrasingTag = exports.isNonPhrasingTag = (0, _tool.makeMap)('NonPhrasing', 'address,article,aside,base,blockquote,body,caption,col,colgroup,dd,' + 'details,dialog,div,dl,dt,fieldset,figcaption,figure,footer,form,' + 'h1,h2,h3,h4,h5,h6,head,header,hgroup,hr,html,legend,li,menuitem,meta,' + 'title,tr,track');

//不可嵌套标签
var canBeleftOpenTag = exports.canBeleftOpenTag = (0, _tool.makeMap)('beleftOpenTag', 'web,spinner,switch,video,textarea,canvas,' + 'indicator,marquee,countdown');

//自闭和标签
var isUnaryTag = exports.isUnaryTag = (0, _tool.makeMap)('unaryTag', 'embed,img,image,input,link,meta,br');

/***/ }),
/* 25 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(11);


/***/ })
/******/ ]);
});
//# sourceMappingURL=vueMini.js.map