var prefix = 'v';
 /**
  * Directives
  */

var Directives = {
    /**
     * 对应于 v-text 指令
     */
    text: function (el, value) {
        el.textContent = value || '';
    },
    show: function (el, value) {
        el.style.display = value ? '' : 'none';
    },
    /**
     * 对应于 v-model 指令
     */
    model: function (el, value, dirAgr, dir, vm, key) {
        var eventName = 'keyup';
        el.value = value || '';

        /**
         * 事件绑定控制
         */
        if (el.handlers && el.handlers[eventName]) {
            el.removeEventListener(eventName, el.handlers[eventName]);
        } else {
            el.handlers = {};
        }

        el.handlers[eventName] = function (e) {
            vm[key] = e.target.value;
        }

        el.addEventListener(eventName, el.handlers[eventName]);
    },
    on: {
        update: function (el, handler, eventName, directive) {
            if (!directive.handlers) {
                directive.handlers = {}
            }

            var handlers = directive.handlers;

            if (handlers[eventName]) {
                //绑定新的事件前移除原绑定的事件函数
                el.removeEventListener(eventName, handlers[eventName]);
            }
            //绑定新的事件函数
            if (handler) {
                handler = handler.bind(el);
                el.addEventListener(eventName, handler);
                handlers[eventName] = handler;
            }
        }
    }
}


/**
 * MiniVue 
 */
function TinyVue (opts) {
    /**
     * root/this.$el: 根节点
     * els: 指令节点
     * bindings: 指令与data关联的桥梁
     */
    var self = this,
        root = this.$el = document.getElementById(opts.el),
        els  = this.$els = root.querySelectorAll(getDirSelectors(Directives)),
        bindings = {};
    this._bindings = bindings;

    /**
     * 指令处理
     */
    [].forEach.call(els, processNode);
    processNode(root);

    /**
     * vm响应式数据初始化
     */

    var _data = extend(opts.data, opts.methods);
    for (var key in bindings) {
        if (bindings.hasOwnProperty(key)) {
            self[key] = _data[key];
        }
    }

    function processNode (el) {
        getAttributes(el.attributes).forEach(function (attr) {
            var directive = parseDirective(attr);
            if (directive) {
                bindDirective(self, el, bindings, directive);
            }
        })
    }

    /**
     * ready
     */
    if (opts.ready && typeof opts.ready == 'function') {
        this.ready = opts.ready;
        this.ready();
    }
}

/**************************************************************
 * @privete
 * helper methods
 */

/**
 * 获取节点属性
 * 'v-text'='counter' => {name: v-text, value: 'counter'}
 */
function getAttributes (attributes) {
    return [].map.call(attributes, function (attr) {
        return {
            name: attr.name,
            value: attr.value
        }
    })
}

/**
 * 返回指令选择器，便于指令节点的查找
 */
function getDirSelectors (directives) {
    /**
     * 支持的事件指令
     */
    var eventArr = ['click', 'change', 'blur']; 


    return Object.keys(directives).map(function (directive) {
        /**
         * text => 'v-text'
         */
        return '[' + prefix + '-' + directive + ']';
    }).join() + ',' + eventArr.map(function (eventName) {
        return '[' + prefix + '-on-' + eventName + ']';
    }).join();
}

/**
 * 节点指令绑定
 */
function bindDirective (vm, el, bindings, directive) {
    //从节点属性中移除指令声明
    el.removeAttribute(directive.attr.name);
    
    /**
     * v-text='counter'
     * v-model='counter'
     * data = { 
            counter: 1 
        } 
     * 这里的 counter 即指令的 key
     */
    var key = directive.key,
        binding = bindings[key];

    if (!binding) {
        /**
         * value 即 counter 对应的值
         * directives 即 key 所绑定的相关指令
         如：
           bindings['counter'] = {
                value: 1,
                directives: [textDirective, modelDirective]
             }
         */
        bindings[key] = binding = {
            value: '',
            directives: []
        }
    }
    directive.el = el;
    binding.directives.push(directive);

    //避免重复定义
    if (!vm.hasOwnProperty(key)) {
        /**
         * get/set 操作绑定
         */
        bindAccessors(vm, key, binding);
    }
}

/**
 * get/set 绑定指令更新操作
 */
function bindAccessors (vm, key, binding) {
    Object.defineProperty(vm, key, {
        get: function () {
            return binding.value;
        },
        set: function (value) {
            binding.value = value;
            binding.directives.forEach(function (directive) {
                directive.update(
                    directive.el,
                    value,
                    directive.argument,
                    directive,
                    vm,
                    key
                )
            })
        }
    })
}

function parseDirective (attr) {
    if (attr.name.indexOf(prefix) === -1) { return ; }

    /**
     * 指令解析
       v-on-click='onClick'
       这里的指令名称为 'on', 'click'为指令的参数，onClick 为key
     */

    //移除 'v-' 前缀, 提取指令名称、指令参数
    var directiveStr = attr.name.slice(prefix.length + 1),
        argIndex = directiveStr.indexOf('-'),
        directiveName = argIndex === -1
            ? directiveStr
            : directiveStr.slice(0, argIndex),
        directiveDef = Directives[directiveName],
        arg = argIndex === -1
            ? null
            : directiveStr.slice(argIndex + 1);

    /**
     * 指令表达式解析，即 v-text='counter' counter的解析
     * 这里暂时只考虑包含key的情况
     */
    var key = attr.value;
    return directiveDef
        ? {
            attr: attr,
            key: key,
            dirname: directiveName,
            definition: directiveDef,
            argument: arg,
            /**
             * 指令本身是一个函数的情况下，更新函数即它本身，否则调用它的update方法
             */
            update: typeof directiveDef === 'function'
                ? directiveDef
                : directiveDef.update
        }
        : null;
}

/**
 * 对象合并
 */
function extend (child, parent) {
    parent = parent || {};
    child = child || {};

    for(var key in parent) {
        if (parent.hasOwnProperty(key)) {
            child[key] = parent[key];
        }
    }

    return child;
}














