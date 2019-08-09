!function(){"use strict";var t=function(){var t=function(t){var e=this,n=0;for(n=0;n<t.length;n++)e[n]=t[n];return e.length=t.length,this},e=function(e,n){var i=[],o=0;if(e&&!n&&e instanceof t)return e;if(e)if("string"==typeof e){var r,s,a=e.trim();if(a.indexOf("<")>=0&&a.indexOf(">")>=0){var l="div";for(0===a.indexOf("<li")&&(l="ul"),0===a.indexOf("<tr")&&(l="tbody"),(0===a.indexOf("<td")||0===a.indexOf("<th"))&&(l="tr"),0===a.indexOf("<tbody")&&(l="table"),0===a.indexOf("<option")&&(l="select"),(s=document.createElement(l)).innerHTML=e,o=0;o<s.childNodes.length;o++)i.push(s.childNodes[o])}else for(r=n||"#"!==e[0]||e.match(/[ .<>:~]/)?(n||document).querySelectorAll(e):[document.getElementById(e.split("#")[1])],o=0;o<r.length;o++)r[o]&&i.push(r[o])}else if(e.nodeType||e===window||e===document)i.push(e);else if(e.length>0&&e[0].nodeType)for(o=0;o<e.length;o++)i.push(e[o]);return new t(i)};t.prototype={addClass:function(t){if(void 0===t)return this;for(var e=t.split(" "),n=0;n<e.length;n++)for(var i=0;i<this.length;i++)void 0!==this[i].classList&&this[i].classList.add(e[n]);return this},removeClass:function(t){for(var e=t.split(" "),n=0;n<e.length;n++)for(var i=0;i<this.length;i++)void 0!==this[i].classList&&this[i].classList.remove(e[n]);return this},hasClass:function(t){return!!this[0]&&this[0].classList.contains(t)},toggleClass:function(t){for(var e=t.split(" "),n=0;n<e.length;n++)for(var i=0;i<this.length;i++)void 0!==this[i].classList&&this[i].classList.toggle(e[n]);return this},attr:function(t,e){if(1===arguments.length&&"string"==typeof t)return this[0]?this[0].getAttribute(t):void 0;for(var n=0;n<this.length;n++)if(2===arguments.length)this[n].setAttribute(t,e);else for(var i in t)this[n][i]=t[i],this[n].setAttribute(i,t[i]);return this},removeAttr:function(t){for(var e=0;e<this.length;e++)this[e].removeAttribute(t);return this},prop:function(t,e){if(1===arguments.length&&"string"==typeof t)return this[0]?this[0][t]:void 0;for(var n=0;n<this.length;n++)if(2===arguments.length)this[n][t]=e;else for(var i in t)this[n][i]=t[i];return this},data:function(t,e){if(void 0!==e){for(var n=0;n<this.length;n++){var i=this[n];i.dom7ElementDataStorage||(i.dom7ElementDataStorage={}),i.dom7ElementDataStorage[t]=e}return this}if(this[0]){if(this[0].dom7ElementDataStorage&&t in this[0].dom7ElementDataStorage)return this[0].dom7ElementDataStorage[t];var o=this[0].getAttribute("data-"+t);return o||void 0}},removeData:function(t){for(var e=0;e<this.length;e++){var n=this[e];n.dom7ElementDataStorage&&n.dom7ElementDataStorage[t]&&(n.dom7ElementDataStorage[t]=null,delete n.dom7ElementDataStorage[t])}},dataset:function(){var t=this[0];if(t){var n={};if(t.dataset)for(var i in t.dataset)n[i]=t.dataset[i];else for(var o=0;o<t.attributes.length;o++){var r=t.attributes[o];r.name.indexOf("data-")>=0&&(n[e.toCamelCase(r.name.split("data-")[1])]=r.value)}for(var s in n)"false"===n[s]?n[s]=!1:"true"===n[s]?n[s]=!0:parseFloat(n[s])===1*n[s]&&(n[s]=1*n[s]);return n}},val:function(t){if(void 0===t)return this[0]?this[0].value:void 0;for(var e=0;e<this.length;e++)this[e].value=t;return this},transform:function(t){for(var e=0;e<this.length;e++){var n=this[e].style;n.webkitTransform=n.MsTransform=n.msTransform=n.MozTransform=n.OTransform=n.transform=t}return this},transition:function(t){"string"!=typeof t&&(t+="ms");for(var e=0;e<this.length;e++){var n=this[e].style;n.webkitTransitionDuration=n.MsTransitionDuration=n.msTransitionDuration=n.MozTransitionDuration=n.OTransitionDuration=n.transitionDuration=t}return this},on:function(t,n,i,o){function r(t){var o=t.target;if(e(o).is(n))i.call(o,t);else for(var r=e(o).parents(),s=0;s<r.length;s++)e(r[s]).is(n)&&i.call(r[s],t)}var s,a,l=t.split(" ");for(s=0;s<this.length;s++)if("function"==typeof n||!1===n)for("function"==typeof n&&(i=arguments[1],o=arguments[2]||!1),a=0;a<l.length;a++)this[s].addEventListener(l[a],i,o);else for(a=0;a<l.length;a++)this[s].dom7LiveListeners||(this[s].dom7LiveListeners=[]),this[s].dom7LiveListeners.push({listener:i,liveListener:r}),this[s].addEventListener(l[a],r,o);return this},off:function(t,e,n,i){for(var o=t.split(" "),r=0;r<o.length;r++)for(var s=0;s<this.length;s++)if("function"==typeof e||!1===e)"function"==typeof e&&(n=arguments[1],i=arguments[2]||!1),this[s].removeEventListener(o[r],n,i);else if(this[s].dom7LiveListeners)for(var a=0;a<this[s].dom7LiveListeners.length;a++)this[s].dom7LiveListeners[a].listener===n&&this[s].removeEventListener(o[r],this[s].dom7LiveListeners[a].liveListener,i);return this},once:function(t,e,n,i){function o(s){n.call(s.target,s),r.off(t,e,o,i)}var r=this;return"function"==typeof e&&(n=arguments[1],i=arguments[2],e=!1),r.on(t,e,o,i)},trigger:function(t,e){for(var n=t.split(" "),i=0;i<n.length;i++)for(var o=0;o<this.length;o++){var r;try{r=new CustomEvent(n[i],{detail:e,bubbles:!0,cancelable:!0})}catch(t){(r=document.createEvent("Event")).initEvent(n[i],!0,!0),r.detail=e}this[o].dispatchEvent(r)}return this},transitionEnd:function(t){function e(r){if(r.target===this)for(t.call(this,r),n=0;n<i.length;n++)o.off(i[n],e)}var n,i=["webkitTransitionEnd","transitionend","oTransitionEnd","MSTransitionEnd","msTransitionEnd"],o=this;if(t)for(n=0;n<i.length;n++)o.on(i[n],e);return this},animationEnd:function(t){function e(r){for(t(r),n=0;n<i.length;n++)o.off(i[n],e)}var n,i=["webkitAnimationEnd","OAnimationEnd","MSAnimationEnd","animationend"],o=this;if(t)for(n=0;n<i.length;n++)o.on(i[n],e);return this},width:function(){return this[0]===window?window.innerWidth:this.length>0?parseFloat(this.css("width")):null},outerWidth:function(t){if(this.length>0){if(t){var e=this.styles();return this[0].offsetWidth+parseFloat(e.getPropertyValue("margin-right"))+parseFloat(e.getPropertyValue("margin-left"))}return this[0].offsetWidth}return null},height:function(){return this[0]===window?window.innerHeight:this.length>0?parseFloat(this.css("height")):null},outerHeight:function(t){if(this.length>0){if(t){var e=this.styles();return this[0].offsetHeight+parseFloat(e.getPropertyValue("margin-top"))+parseFloat(e.getPropertyValue("margin-bottom"))}return this[0].offsetHeight}return null},offset:function(){if(this.length>0){var t=this[0],e=t.getBoundingClientRect(),n=document.body,i=t.clientTop||n.clientTop||0,o=t.clientLeft||n.clientLeft||0,r=window.pageYOffset||t.scrollTop,s=window.pageXOffset||t.scrollLeft;return{top:e.top+r-i,left:e.left+s-o}}return null},hide:function(){for(var t=0;t<this.length;t++)this[t].style.display="none";return this},show:function(){for(var t=0;t<this.length;t++)this[t].style.display="block";return this},styles:function(){return this[0]?window.getComputedStyle(this[0],null):void 0},css:function(t,e){var n;if(1===arguments.length){if("string"!=typeof t){for(n=0;n<this.length;n++)for(var i in t)this[n].style[i]=t[i];return this}if(this[0])return window.getComputedStyle(this[0],null).getPropertyValue(t)}if(2===arguments.length&&"string"==typeof t){for(n=0;n<this.length;n++)this[n].style[t]=e;return this}return this},each:function(t){for(var e=0;e<this.length;e++)t.call(this[e],e,this[e]);return this},filter:function(e){for(var n=[],i=this,o=0;o<i.length;o++)e.call(i[o],o,i[o])&&n.push(i[o]);return new t(n)},html:function(t){if(void 0===t)return this[0]?this[0].innerHTML:void 0;for(var e=0;e<this.length;e++)this[e].innerHTML=t;return this},text:function(t){if(void 0===t)return this[0]?this[0].textContent.trim():null;for(var e=0;e<this.length;e++)this[e].textContent=t},is:function(n){if(!this[0]||void 0===n)return!1;var i,o;if("string"==typeof n){var r=this[0];if(r===document)return n===document;if(r===window)return n===window;if(r.matches)return r.matches(n);if(r.webkitMatchesSelector)return r.webkitMatchesSelector(n);if(r.mozMatchesSelector)return r.mozMatchesSelector(n);if(r.msMatchesSelector)return r.msMatchesSelector(n);for(i=e(n),o=0;o<i.length;o++)if(i[o]===this[0])return!0;return!1}if(n===document)return this[0]===document;if(n===window)return this[0]===window;if(n.nodeType||n instanceof t){for(i=n.nodeType?[n]:n,o=0;o<i.length;o++)if(i[o]===this[0])return!0;return!1}return!1},indexOf:function(t){for(var e=0;e<this.length;e++)if(this[e]===t)return e},index:function(){if(this[0]){for(var t=this[0],e=0;null!==(t=t.previousSibling);)1===t.nodeType&&e++;return e}},eq:function(e){if(void 0===e)return this;var n,i=this.length;return e>i-1?new t([]):0>e?(n=i+e,new t(0>n?[]:[this[n]])):new t([this[e]])},append:function(e){var n,i;for(n=0;n<this.length;n++)if("string"==typeof e){var o=document.createElement("div");for(o.innerHTML=e;o.firstChild;)this[n].appendChild(o.firstChild)}else if(e instanceof t)for(i=0;i<e.length;i++)this[n].appendChild(e[i]);else this[n].appendChild(e);return this},appendTo:function(t){return e(t).append(this),this},prepend:function(e){var n,i;for(n=0;n<this.length;n++)if("string"==typeof e){var o=document.createElement("div");for(o.innerHTML=e,i=o.childNodes.length-1;i>=0;i--)this[n].insertBefore(o.childNodes[i],this[n].childNodes[0])}else if(e instanceof t)for(i=0;i<e.length;i++)this[n].insertBefore(e[i],this[n].childNodes[0]);else this[n].insertBefore(e,this[n].childNodes[0]);return this},prependTo:function(t){return e(t).prepend(this),this},insertBefore:function(t){for(var n=e(t),i=0;i<this.length;i++)if(1===n.length)n[0].parentNode.insertBefore(this[i],n[0]);else if(n.length>1)for(var o=0;o<n.length;o++)n[o].parentNode.insertBefore(this[i].cloneNode(!0),n[o])},insertAfter:function(t){for(var n=e(t),i=0;i<this.length;i++)if(1===n.length)n[0].parentNode.insertBefore(this[i],n[0].nextSibling);else if(n.length>1)for(var o=0;o<n.length;o++)n[o].parentNode.insertBefore(this[i].cloneNode(!0),n[o].nextSibling)},next:function(n){return new t(this.length>0?n?this[0].nextElementSibling&&e(this[0].nextElementSibling).is(n)?[this[0].nextElementSibling]:[]:this[0].nextElementSibling?[this[0].nextElementSibling]:[]:[])},nextAll:function(n){var i=[],o=this[0];if(!o)return new t([]);for(;o.nextElementSibling;){var r=o.nextElementSibling;n?e(r).is(n)&&i.push(r):i.push(r),o=r}return new t(i)},prev:function(n){return new t(this.length>0?n?this[0].previousElementSibling&&e(this[0].previousElementSibling).is(n)?[this[0].previousElementSibling]:[]:this[0].previousElementSibling?[this[0].previousElementSibling]:[]:[])},prevAll:function(n){var i=[],o=this[0];if(!o)return new t([]);for(;o.previousElementSibling;){var r=o.previousElementSibling;n?e(r).is(n)&&i.push(r):i.push(r),o=r}return new t(i)},siblings:function(t){return this.nextAll(t).add(this.prevAll(t))},parent:function(t){for(var n=[],i=0;i<this.length;i++)null!==this[i].parentNode&&(t?e(this[i].parentNode).is(t)&&n.push(this[i].parentNode):n.push(this[i].parentNode));return e(e.unique(n))},parents:function(t){for(var n=[],i=0;i<this.length;i++)for(var o=this[i].parentNode;o;)t?e(o).is(t)&&n.push(o):n.push(o),o=o.parentNode;return e(e.unique(n))},find:function(e){for(var n=[],i=0;i<this.length;i++)for(var o=this[i].querySelectorAll(e),r=0;r<o.length;r++)n.push(o[r]);return new t(n)},children:function(n){for(var i=[],o=0;o<this.length;o++)for(var r=this[o].childNodes,s=0;s<r.length;s++)n?1===r[s].nodeType&&e(r[s]).is(n)&&i.push(r[s]):1===r[s].nodeType&&i.push(r[s]);return new t(e.unique(i))},remove:function(){for(var t=0;t<this.length;t++)this[t].parentNode&&this[t].parentNode.removeChild(this[t]);return this},detach:function(){return this.remove()},add:function(){var t,n,i=this;for(t=0;t<arguments.length;t++){var o=e(arguments[t]);for(n=0;n<o.length;n++)i[i.length]=o[n],i.length++}return i}},function(){function n(n){t.prototype[n]=function(t,i,r){var s;if(void 0===t){for(s=0;s<this.length;s++)o.indexOf(n)<0&&(n in this[s]?this[s][n]():e(this[s]).trigger(n));return this}return this.on(n,t,i,r)}}for(var i="click blur focus focusin focusout keyup keydown keypress submit change mousedown mousemove mouseup mouseenter mouseleave mouseout mouseover touchstart touchend touchmove resize scroll".split(" "),o="resize scroll".split(" "),r=0;r<i.length;r++)n(i[r])}();var n={};e.ajaxSetup=function(t){t.type&&(t.method=t.type),e.each(t,function(t,e){n[t]=e})};var i=0;return e.ajax=function(t){function o(i,o,r){var s=arguments;i&&e(document).trigger(i,o),r&&(r in n&&n[r](s[3],s[4],s[5],s[6]),t[r]&&t[r](s[3],s[4],s[5],s[6]))}var r={method:"GET",data:!1,async:!0,cache:!0,user:"",password:"",headers:{},xhrFields:{},statusCode:{},processData:!0,dataType:"text",contentType:"application/x-www-form-urlencoded",timeout:0},s=["beforeSend","error","complete","success","statusCode"];t.type&&(t.method=t.type),e.each(n,function(t,e){s.indexOf(t)<0&&(r[t]=e)}),e.each(r,function(e,n){e in t||(t[e]=n)}),t.url||(t.url=window.location.toString());var a=t.url.indexOf("?")>=0?"&":"?",l=t.method.toUpperCase();if(("GET"===l||"HEAD"===l||"OPTIONS"===l||"DELETE"===l)&&t.data){var u;(u="string"==typeof t.data?t.data.indexOf("?")>=0?t.data.split("?")[1]:t.data:e.serializeObject(t.data)).length&&(t.url+=a+u,"?"===a&&(a="&"))}if("json"===t.dataType&&t.url.indexOf("callback=")>=0){var d,c="f7jsonp_"+Date.now()+i++,p=t.url.split("callback="),f=p[0]+"callback="+c;if(p[1].indexOf("&")>=0){var h=p[1].split("&").filter(function(t){return t.indexOf("=")>0}).join("&");h.length>0&&(f+="&"+h)}var m=document.createElement("script");return m.type="text/javascript",m.onerror=function(){clearTimeout(d),o(void 0,void 0,"error",null,"scripterror")},m.src=f,window[c]=function(t){clearTimeout(d),o(void 0,void 0,"success",t),m.parentNode.removeChild(m),m=null,delete window[c]},document.querySelector("head").appendChild(m),void(t.timeout>0&&(d=setTimeout(function(){m.parentNode.removeChild(m),m=null,o(void 0,void 0,"error",null,"timeout")},t.timeout)))}("GET"===l||"HEAD"===l||"OPTIONS"===l||"DELETE"===l)&&!1===t.cache&&(t.url+=a+"_nocache="+Date.now());var v=new XMLHttpRequest;v.requestUrl=t.url,v.requestParameters=t,v.open(l,t.url,t.async,t.user,t.password);var g=null;if(("POST"===l||"PUT"===l||"PATCH"===l)&&t.data)if(t.processData)if([ArrayBuffer,Blob,Document,FormData].indexOf(t.data.constructor)>=0)g=t.data;else{var T="---------------------------"+Date.now().toString(16);"multipart/form-data"===t.contentType?v.setRequestHeader("Content-Type","multipart/form-data; boundary="+T):v.setRequestHeader("Content-Type",t.contentType),g="";var w=e.serializeObject(t.data);if("multipart/form-data"===t.contentType){T="---------------------------"+Date.now().toString(16),w=w.split("&");for(var y=[],x=0;x<w.length;x++)y.push('Content-Disposition: form-data; name="'+w[x].split("=")[0]+'"\r\n\r\n'+w[x].split("=")[1]+"\r\n");g="--"+T+"\r\n"+y.join("--"+T+"\r\n")+"--"+T+"--\r\n"}else g="application/x-www-form-urlencoded"===t.contentType?w:w.replace(/&/g,"\r\n")}else g=t.data;t.headers&&e.each(t.headers,function(t,e){v.setRequestHeader(t,e)}),void 0===t.crossDomain&&(t.crossDomain=/^([\w-]+:)?\/\/([^\/]+)/.test(t.url)&&RegExp.$2!==window.location.host),t.crossDomain||v.setRequestHeader("X-Requested-With","XMLHttpRequest"),t.xhrFields&&e.each(t.xhrFields,function(t,e){v[t]=e});var S;return v.onload=function(e){if(S&&clearTimeout(S),v.status>=200&&v.status<300||0===v.status){var i;if("json"===t.dataType)try{i=JSON.parse(v.responseText),o("ajaxSuccess",{xhr:v},"success",i,v.status,v)}catch(t){o("ajaxError",{xhr:v,parseerror:!0},"error",v,"parseerror")}else i="text"===v.responseType||""===v.responseType?v.responseText:v.response,o("ajaxSuccess",{xhr:v},"success",i,v.status,v)}else o("ajaxError",{xhr:v},"error",v,v.status);t.statusCode&&(n.statusCode&&n.statusCode[v.status]&&n.statusCode[v.status](v),t.statusCode[v.status]&&t.statusCode[v.status](v)),o("ajaxComplete",{xhr:v},"complete",v,v.status)},v.onerror=function(t){S&&clearTimeout(S),o("ajaxError",{xhr:v},"error",v,v.status)},o("ajaxStart",{xhr:v},"start",v),o(void 0,void 0,"beforeSend",v),v.send(g),t.timeout>0&&(v.onabort=function(){S&&clearTimeout(S)},S=setTimeout(function(){v.abort(),o("ajaxError",{xhr:v,timeout:!0},"error",v,"timeout"),o("ajaxComplete",{xhr:v,timeout:!0},"complete",v,"timeout")},t.timeout)),v},function(){function t(t){e[t]=function(n,i,o){return e.ajax({url:n,method:"post"===t?"POST":"GET",data:"function"==typeof i?void 0:i,success:"function"==typeof i?i:o,dataType:"getJSON"===t?"json":void 0})}}for(var n="get post getJSON".split(" "),i=0;i<n.length;i++)t(n[i])}(),e.parseUrlQuery=function(t){var e,n,i,o={};if(!(t.indexOf("?")>=0))return o;for(n=(t=t.split("?")[1]).split("&"),e=0;e<n.length;e++)i=n[e].split("="),o[i[0]]=i[1];return o},e.isArray=function(t){return"[object Array]"===Object.prototype.toString.apply(t)},e.each=function(n,i){if("object"==typeof n&&i){var o,r;if(e.isArray(n)||n instanceof t)for(o=0;o<n.length;o++)i(o,n[o]);else for(r in n)n.hasOwnProperty(r)&&i(r,n[r])}},e.unique=function(t){for(var e=[],n=0;n<t.length;n++)-1===e.indexOf(t[n])&&e.push(t[n]);return e},e.serializeObject=e.param=function(t,n){function i(t){if(n.length>0){for(var e="",i=0;i<n.length;i++)e+=0===i?n[i]:"["+encodeURIComponent(n[i])+"]";return e+"["+encodeURIComponent(t)+"]"}return encodeURIComponent(t)}function o(t){return encodeURIComponent(t)}if("string"==typeof t)return t;var r=[],s="&";n=n||[];var a;for(var l in t)if(t.hasOwnProperty(l)){var u;if(e.isArray(t[l])){u=[];for(var d=0;d<t[l].length;d++)e.isArray(t[l][d])||"object"!=typeof t[l][d]?u.push(i(l)+"[]="+o(t[l][d])):((a=n.slice()).push(l),a.push(d+""),u.push(e.serializeObject(t[l][d],a)));u.length>0&&r.push(u.join(s))}else"object"==typeof t[l]?((a=n.slice()).push(l),""!==(u=e.serializeObject(t[l],a))&&r.push(u)):void 0!==t[l]&&""!==t[l]&&r.push(i(l)+"="+o(t[l]))}return r.join(s)},e.toCamelCase=function(t){return t.toLowerCase().replace(/-(.)/g,function(t,e){return e.toUpperCase()})},e.dataset=function(t){return e(t).dataset()},e.getTranslate=function(t,e){var n,i,o,r;return void 0===e&&(e="x"),o=window.getComputedStyle(t,null),window.WebKitCSSMatrix?((i=o.transform||o.webkitTransform).split(",").length>6&&(i=i.split(", ").map(function(t){return t.replace(",",".")}).join(", ")),r=new WebKitCSSMatrix("none"===i?"":i)):(r=o.MozTransform||o.OTransform||o.MsTransform||o.msTransform||o.transform||o.getPropertyValue("transform").replace("translate(","matrix(1, 0, 0, 1,"),n=r.toString().split(",")),"x"===e&&(i=window.WebKitCSSMatrix?r.m41:16===n.length?parseFloat(n[12]):parseFloat(n[4])),"y"===e&&(i=window.WebKitCSSMatrix?r.m42:16===n.length?parseFloat(n[13]):parseFloat(n[5])),i||0},e.requestAnimationFrame=function(t){return window.requestAnimationFrame?window.requestAnimationFrame(t):window.webkitRequestAnimationFrame?window.webkitRequestAnimationFrame(t):window.mozRequestAnimationFrame?window.mozRequestAnimationFrame(t):window.setTimeout(t,1e3/60)},e.cancelAnimationFrame=function(t){return window.cancelAnimationFrame?window.cancelAnimationFrame(t):window.webkitCancelAnimationFrame?window.webkitCancelAnimationFrame(t):window.mozCancelAnimationFrame?window.mozCancelAnimationFrame(t):window.clearTimeout(t)},e.supportTouch=!!("ontouchstart"in window||window.DocumentTouch&&document instanceof DocumentTouch),e.fn=t.prototype,e.fn.scrollTo=function(t,n,i,o,r){return 4===arguments.length&&"function"==typeof o&&(r=o,o=void 0),this.each(function(){function s(t){void 0===t&&(t=(new Date).getTime()),null===T&&(T=t);var n,u=Math.max(Math.min((t-T)/i,1),0),d="linear"===o?u:.5-Math.cos(u*Math.PI)/2;return v&&(f=a+d*(c-a)),g&&(h=l+d*(p-l)),v&&c>a&&f>=c&&(m.scrollTop=c,n=!0),v&&a>c&&c>=f&&(m.scrollTop=c,n=!0),g&&p>l&&h>=p&&(m.scrollLeft=p,n=!0),g&&l>p&&p>=h&&(m.scrollLeft=p,n=!0),n?void(r&&r()):(v&&(m.scrollTop=f),g&&(m.scrollLeft=h),void e.requestAnimationFrame(s))}var a,l,u,d,c,p,f,h,m=this,v=n>0||0===n,g=t>0||0===t;if(void 0===o&&(o="swing"),v&&(a=m.scrollTop,i||(m.scrollTop=n)),g&&(l=m.scrollLeft,i||(m.scrollLeft=t)),i){v&&(u=m.scrollHeight-m.offsetHeight,c=Math.max(Math.min(n,u),0)),g&&(d=m.scrollWidth-m.offsetWidth,p=Math.max(Math.min(t,d),0));var T=null;v&&c===a&&(v=!1),g&&p===l&&(g=!1),e.requestAnimationFrame(s)}})},e.fn.scrollTop=function(t,e,n,i){3===arguments.length&&"function"==typeof n&&(i=n,n=void 0);var o=this;return void 0===t?o.length>0?o[0].scrollTop:null:o.scrollTo(void 0,t,e,n,i)},e.fn.scrollLeft=function(t,e,n,i){3===arguments.length&&"function"==typeof n&&(i=n,n=void 0);var o=this;return void 0===t?o.length>0?o[0].scrollLeft:null:o.scrollTo(t,void 0,e,n,i)},e}();window.smartms=t}(),Swiper=function(t,e,n){function i(t){return document.querySelectorAll(t)}function o(){var t=h-p*e.slidesPerSlide;return e.loop&&(t-=g),t}function r(){d.init(),d.swipeTo(d.activeSlide,0,!1)}function s(t){if(d.isTouched||e.onlyExternal)return!1;if(!t.assignedToSwiper&&(t.assignedToSwiper=!0,d.isTouched=!0,!d.isSupportTouch()||1==t.targetTouches.length)){e.loop&&d.fixLoop(),d.isSupportTouch()||t.preventDefault(),d.touches.startX=d.touches.currentX=d.isSupportTouch()?t.targetTouches[0].pageX:t.pageX,d.touches.startY=d.touches.currentY=d.isSupportTouch()?t.targetTouches[0].pageY:t.pageY,d.touches.start=d.touches.current=c?d.touches.startX:d.touches.startY,d.setTransition(0),d.positions.start=d.positions.current=c?d.getTranslate("x"):d.getTranslate("y"),c?d.setTransform(d.positions.start,0,0):d.setTransform(0,d.positions.start,0);var n=new Date;d.times.start=n.getTime(),v=void 0,e.onTouchStart&&e.onTouchStart(d)}}function a(t){if(d.isTouched&&!e.onlyExternal&&!(d.isSupportTouch()&&(void 0===v&&c&&(v=!!(v||Math.abs(t.targetTouches[0].pageY-d.touches.startY)>Math.abs(t.targetTouches[0].pageX-d.touches.startX))),void 0!==v||c||(v=!!(v||Math.abs(t.targetTouches[0].pageY-d.touches.startY)<Math.abs(t.targetTouches[0].pageX-d.touches.startX))),v)||t.assignedToSwiper||(t.assignedToSwiper=!0,e.autoPlay&&d.stopAutoPlay(),d.isSupportTouch()&&1!=t.touches.length))){if(t.preventDefault(),e.onTouchMove&&e.onTouchMove(d),d.touches.current=c?d.isSupportTouch()?t.targetTouches[0].pageX:t.pageX:d.isSupportTouch()?t.targetTouches[0].pageY:t.pageY,d.positions.current=(d.touches.current-d.touches.start)*e.ratio+d.positions.start,d.positions.current>0&&(!e.freeMode||e.freeModeFluid)){if(e.loop){n=1;d.positions.current>0&&(d.positions.current=0)}else n=(2*g-d.positions.current)/g/2;d.positions.current=n<.5?g/2:d.positions.current*n}if(Math.abs(d.positions.current)>h-p*e.slidesPerSlide&&(!e.freeMode||e.freeModeFluid)){if(e.loop)var n=1,i=d.positions.current,r=-o()-g;else var s=(d.touches.current-d.touches.start)*e.ratio+(o()+d.positions.start),n=(g+s)/g,i=d.positions.current-s*(1-n)/2,r=-o()-g/2;d.positions.current=i<r||n<=0?r:i}if(!e.followFinger)return;c?d.setTransform(d.positions.current,0,0):d.setTransform(0,d.positions.current,0),e.freeMode&&d.updateActiveSlide(d.positions.current)}}function l(t){if(!e.onlyExternal&&d.isTouched){d.isTouched=!1,d.positions.current||0===d.positions.current||(d.positions.current=d.positions.start),c?d.setTransform(d.positions.current,0,0):d.setTransform(0,d.positions.current,0);var n=new Date;d.times.end=n.getTime(),d.touches.diff=d.touches.current-d.touches.start,d.touches.abs=Math.abs(d.touches.diff),d.positions.diff=d.positions.current-d.positions.start,d.positions.abs=Math.abs(d.positions.diff);var i=d.positions.diff,o=d.positions.abs;o<5&&d.swipeReset();var r=h-p*e.slidesPerSlide;if(d.positions.current>0)return d.swipeReset(),void(e.onTouchEnd&&e.onTouchEnd(d));if(Math.abs(d.positions.current)>r)return d.swipeReset(),void(e.onTouchEnd&&e.onTouchEnd(d));if(e.freeMode){if(d.times.end-d.times.start<300&&e.freeModeFluid){var s=d.positions.current+2*d.touches.diff;s<-1*r&&(s=-r),s>0&&(s=0),c?d.setTransform(s,0,0):d.setTransform(0,s,0),d.setTransition(2*(d.times.end-d.times.start)),d.updateActiveSlide(s)}return(!e.freeModeFluid||d.times.end-d.times.start>=300)&&d.updateActiveSlide(d.positions.current),void(e.onTouchEnd&&e.onTouchEnd(d))}"toNext"==(m=i<0?"toNext":"toPrev")&&d.times.end-d.times.start<=300&&(o<30?d.swipeReset():d.swipeNext(!0)),"toPrev"==m&&d.times.end-d.times.start<=300&&(o<30?d.swipeReset():d.swipePrev(!0)),"toNext"==m&&d.times.end-d.times.start>300&&(o>=.5*p?d.swipeNext(!0):d.swipeReset()),"toPrev"==m&&d.times.end-d.times.start>300&&(o>=.5*p?d.swipePrev(!0):d.swipeReset()),e.onTouchEnd&&e.onTouchEnd(d)}}function u(){e.onSlideChangeStart&&e.onSlideChangeStart(d),e.onSlideChangeEnd&&d.transitionEnd(e.onSlideChangeEnd)}if(document.querySelectorAll&&0!=document.querySelectorAll(t).length){var d=this;d.touches={},d.positions={current:0},d.times={},d.isTouched=!1,d.realIndex=0,d.activeSlide=0,d.previousSlide=null,d.use3D=d.isSupport3D(),e=e||{},d.params=e,e.mode=e.mode||"horizontal",e.ratio=e.ratio||1,e.speed=e.speed||300,e.freeMode=e.freeMode||!1,e.freeModeFluid=e.freeModeFluid||!1,e.slidesPerSlide=e.slidesPerSlide||1,!1===e.simulateTouch?e.simulateTouch=!1:e.simulateTouch=!0,!1===e.followFinger?e.followFinger=!1:e.followFinger=!0,e.autoPlay=e.autoPlay||!1,e.onlyExternal=e.onlyExternal||!1,!1===e.createPagination?e.createPagination=!1:e.createPagination=!0,e.pagination=e.pagination||!1,e.slideClass=e.slideClass||"swiper-slide",e.wrapperClass=e.wrapperClass||"swiper-wrapper",e.paginationClass=e.paginationClass||"swiper-pagination-switch",e.paginationActiveClass=e.paginationActiveClass||"swiper-active-switch";var c,p,f,h,m,v,g,T=i(t+" ."+e.wrapperClass).item(0);d.wrapper=T,c="horizontal"==e.mode;var w={touchStart:d.isSupportTouch()||!e.simulateTouch?"touchstart":"mousedown",touchMove:d.isSupportTouch()||!e.simulateTouch?"touchmove":"mousemove",touchEnd:d.isSupportTouch()||!e.simulateTouch?"touchend":"mouseup"};if(e.loop&&(!function(){f=i(t+" > ."+e.wrapperClass+" > ."+e.slideClass).length;for(var n="",o="",r=0;r<e.slidesPerSlide;r++)n+=i(t+" > ."+e.wrapperClass+" > ."+e.slideClass).item(r).outerHTML;for(r=f-e.slidesPerSlide;r<f;r++)o+=i(t+" > ."+e.wrapperClass+" > ."+e.slideClass).item(r).outerHTML;T.innerHTML=o+T.innerHTML+n}(),setTimeout(function(){d.swipeTo(0,0)},0)),d.init=function(){var n=i(t).item(0).offsetWidth,o=i(t).item(0).offsetHeight;p=g=c?n:o,f=i(t+" > ."+e.wrapperClass+" > ."+e.slideClass).length;for(var r=c?1:e.slidesPerSlide,s=c?e.slidesPerSlide:1,a=0;a<f;a++){var l=i(t+" > ."+e.wrapperClass+" > ."+e.slideClass).item(a);l.style.width=n/s+"px",l.style.height=o/r+"px",e.onSlideInitialize&&e.onSlideInitialize(d,l)}var u=f*n/s,m=f*o/r;h=c?u:m,c?T.style.width=u+"px":T.style.height=m+"px",e.slidesPerSlide&&e.slidesPerSlide>1&&(p/=e.slidesPerSlide)},d.init(),e.pagination&&e.createPagination){for(var y="",x=e.loop?f-2*e.slidesPerSlide:f,S=0;S<x;S++)y+='<span class="'+e.paginationClass+'"></span>';i(e.pagination)[0].innerHTML=y,setTimeout(function(){d.updatePagination()},0)}e.disableAutoResize||window.addEventListener("resize",r,!1);var b;d.startAutoPlay=function(){e.autoPlay&&(b=setInterval(function(){var t=d.realIndex+1;t==f&&(t=0),d.swipeTo(t)},e.autoPlay))},d.stopAutoPlay=function(){b&&clearInterval(b)},e.autoPlay&&d.startAutoPlay(),T.addEventListener(w.touchStart,s,!1);var C=d.isSupportTouch()?T:document;C.addEventListener(w.touchMove,a,!1),C.addEventListener(w.touchEnd,l,!1),d.destroy=function(t){(t=!1===t?t:t||!0)&&window.removeEventListener("resize",r,!1),T.removeEventListener(w.touchStart,s,!1),C.removeEventListener(w.touchMove,a,!1),C.removeEventListener(w.touchEnd,l,!1)},d.swipeNext=function(t){!t&&e.loop&&d.fixLoop();var n=c?d.getTranslate("x"):d.getTranslate("y"),i=Math.floor(Math.abs(n)/Math.floor(p))*p+p;if(i!=h&&(!(i>o())||e.loop))return e.loop&&i>=o()+g&&(i=o()+g),c?d.setTransform(-i,0,0):d.setTransform(0,-i,0),d.setTransition(e.speed),d.updateActiveSlide(-i),u(),!0},d.swipePrev=function(t){!t&&e.loop&&d.fixLoop();var n=c?d.getTranslate("x"):d.getTranslate("y"),i=(Math.ceil(-n/p)-1)*p;return i<0&&(i=0),c?d.setTransform(-i,0,0):d.setTransform(0,-i,0),d.setTransition(e.speed),d.updateActiveSlide(-i),u(),!0},d.swipeReset=function(t){var n=c?d.getTranslate("x"):d.getTranslate("y"),i=n<0?Math.round(n/p)*p:0,r=-o();return i<=r&&(i=r),"horizontal"==e.mode?d.setTransform(i,0,0):d.setTransform(0,i,0),d.setTransition(e.speed),d.updateActiveSlide(i),e.onSlideReset&&e.onSlideReset(d),!0},d.swipeTo=function(t,n,i){if(!(t>f-1)&&(!(t<0)||e.loop)){i=!1!==i&&(i||!0);var n=0===n?n:n||e.speed;e.loop&&(t+=e.slidesPerSlide),t>f-e.slidesPerSlide&&(t=f-e.slidesPerSlide);var o=-t*p;return c?d.setTransform(o,0,0):d.setTransform(0,o,0),d.setTransition(n),d.updateActiveSlide(o),i&&u(),!0}},d.updateActiveSlide=function(t){d.previousSlide=d.realIndex,d.realIndex=Math.round(-t/p),e.loop?(d.activeSlide=d.realIndex-e.slidesPerSlide,d.activeSlide>=f-2*e.slidesPerSlide&&(d.activeSlide=f-2*e.slidesPerSlide-d.activeSlide),d.activeSlide<0&&(d.activeSlide=f-2*e.slidesPerSlide+d.activeSlide)):d.activeSlide=d.realIndex,d.realIndex==f&&(d.realIndex=f-1),d.realIndex<0&&(d.realIndex=0),e.pagination&&d.updatePagination()},d.updatePagination=function(){var t=i(e.pagination+" ."+e.paginationActiveClass);if(t){for(s=0;s<t.length;s++)t.item(s).className.indexOf("active")>=0&&(t.item(s).className=t.item(s).className.replace(e.paginationActiveClass,""));for(var n=i(e.pagination+" ."+e.paginationClass).length,o=e.loop?d.realIndex-e.slidesPerSlide:d.realIndex,r=o+(e.slidesPerSlide-1),s=o;s<=r;s++){var a=s;a>=n&&(a-=n),a<0&&(a=n+a),a<f&&(i(e.pagination+" ."+e.paginationClass).item(a).className=i(e.pagination+" ."+e.paginationClass).item(a).className+" "+e.paginationActiveClass)}}},d.fixLoop=function(){if(d.realIndex<e.slidesPerSlide){t=f-3*e.slidesPerSlide+d.realIndex;d.swipeTo(t,0)}if(d.realIndex>f-2*e.slidesPerSlide){var t=-f+d.realIndex+e.slidesPerSlide;d.swipeTo(t,0)}}}},Swiper.prototype={transitionEnd:function(t){function e(){t(n);for(var r=0;r<o.length;r++)i.removeEventListener(o[r],e,!1)}var n=this,i=n.wrapper,o=["webkitTransitionEnd","transitionend","oTransitionEnd","MSTransitionEnd","msTransitionEnd"];if(t)for(var r=0;r<o.length;r++)i.addEventListener(o[r],e,!1)},isSupportTouch:function(){return"ontouchstart"in window||window.DocumentTouch&&document instanceof DocumentTouch},isSupport3D:function(){var t=document.createElement("div");t.id="test3d";var e=!1;if("webkitPerspective"in t.style&&(e=!0),"MozPerspective"in t.style&&(e=!0),"OPerspective"in t.style&&(e=!0),"MsPerspective"in t.style&&(e=!0),"perspective"in t.style&&(e=!0),e&&"webkitPerspective"in t.style){var n=document.createElement("style");n.textContent="@media (-webkit-transform-3d), (transform-3d), (-moz-transform-3d), (-o-transform-3d), (-ms-transform-3d) {#test3d{height:5px}}",document.getElementsByTagName("head")[0].appendChild(n),document.body.appendChild(t),e=5===t.offsetHeight,n.parentNode.removeChild(n),t.parentNode.removeChild(t)}return e},getTranslate:function(t){var e,n,i=this.wrapper;if(window.WebKitCSSMatrix)e=(o=new WebKitCSSMatrix(window.getComputedStyle(i,null).webkitTransform)).toString().split(",");else{var o=window.getComputedStyle(i,null).MozTransform||window.getComputedStyle(i,null).OTransform||window.getComputedStyle(i,null).MsTransform||window.getComputedStyle(i,null).msTransform||window.getComputedStyle(i,null).transform;e=o.toString().split(",")}return"x"==t&&(n=16==e.length?parseInt(e[12],10):parseInt(e[4],10)),"y"==t&&(n=16==e.length?parseInt(e[13],10):parseInt(e[5],10)),n},setTransform:function(t,e,n){var i=this.wrapper.style;t=t||0,e=e||0,n=n||0,this.use3D?i.webkitTransform=i.MsTransform=i.msTransform=i.MozTransform=i.OTransform=i.transform="translate3d("+t+"px, "+e+"px, "+n+"px)":i.webkitTransform=i.MsTransform=i.msTransform=i.MozTransform=i.OTransform=i.transform="translate("+t+"px, "+e+"px)"},setTransition:function(t){var e=this.wrapper.style;e.webkitTransitionDuration=e.MsTransitionDuration=e.msTransitionDuration=e.MozTransitionDuration=e.OTransitionDuration=e.transitionDuration=t/1e3+"s"}},"undefined"==typeof smartms&&(smartms={}),smartms.dialogs=function(t){var e=this;e.params={modalTemplate:'<div class="modal {{noButtons}}"><div class="modal-inner">{{if title}}<div class="modal-title">{{title}}</div>{{/if title}}<div class="modal-text">{{text}}</div>{{afterText}}</div><div class="modal-buttons">{{buttons}}</div></div>',modalActionsTemplate:'<div class="actions-modal">{{buttons}}</div>',modalButtonOk:"OK",modalButtonCancel:"Cancel",modalTitle:"SmartMS",modalCloseByOutside:!1,modalActionsCloseByOutside:!0,modalPreloaderText:"Loading... ",modalPreloaderTitle:"Loading... ",modalUsernamePlaceholder:"Username",modalPasswordPlaceholder:"Password",popupCloseByOutside:!0,popoverCloseByOutside:!0,modalStack:!0,modalsMoveToRoot:!0,material:!1,materialPreloaderSvg:'<svg xmlns="http://www.w3.org/2000/svg" height="75" width="75" viewbox="0 0 75 75"><circle cx="37.5" cy="37.5" r="33.5" stroke-width="8"/></svg>',materialPreloaderHtml:'<span class="preloader-inner"><span class="preloader-inner-gap"></span><span class="preloader-inner-left"><span class="preloader-inner-half-circle"></span></span><span class="preloader-inner-right"><span class="preloader-inner-half-circle"></span></span></span>'};for(var n in t)e.params[n]=t[n];e._modalTemlateTempDiv=document.createElement("div"),e.modal=function(t){var n="";if((t=t||{}).buttons&&t.buttons.length>0)for(var i=0;i<t.buttons.length;i++)n+='<span class="modal-button'+(t.buttons[i].bold?" modal-button-bold":"")+'">'+t.buttons[i].text+"</span>";var o=e.params.modalTemplate,r=(o=t.title?o.replace(/{{if\ title}}/g,"").replace(/{{\/if\ title}}/g,""):o.split("{{if title}}")[0]+o.split("{{/if title}}")[1]).replace(/{{title}}/g,t.title||"").replace(/{{text}}/g,t.text||"").replace(/{{afterText}}/g,t.afterText||"").replace(/{{buttons}}/g,n).replace(/{{noButtons}}/g,t.buttons&&0!==t.buttons.length?"":"modal-no-buttons");e._modalTemlateTempDiv.innerHTML=r;var s=smartms(e._modalTemlateTempDiv).children();return smartms("body").append(s[0]),s.find(".modal-button").each(function(n,i){smartms(i).on("click",function(i){!1!==t.buttons[n].close&&e.closeModal(s),t.buttons[n].onClick&&t.buttons[n].onClick(s,i)})}),e.openModal(s),s[0]},e.alert=function(t,n,i){return"function"==typeof n&&(i=arguments[1],n=void 0),e.modal({text:t||"",title:void 0===n?e.params.modalTitle:n,buttons:[{text:e.params.modalButtonOk,bold:!0,onClick:i}]})},e.confirm=function(t,n,i,o){return"function"==typeof n&&(o=arguments[2],i=arguments[1],n=void 0),e.modal({text:t||"",title:void 0===n?e.params.modalTitle:n,buttons:[{text:e.params.modalButtonCancel,onClick:o},{text:e.params.modalButtonOk,bold:!0,onClick:i}]})},e.prompt=function(t,n,i,o){return"function"==typeof n&&(o=arguments[2],i=arguments[1],n=void 0),e.modal({text:t||"",title:void 0===n?e.params.modalTitle:n,afterText:'<input type="text" class="modal-prompt-input">',buttons:[{text:e.params.modalButtonCancel,onClick:function(t){o&&o(smartms(t).find(".modal-prompt-input").val())}},{text:e.params.modalButtonOk,bold:!0,onClick:function(t){i&&i(smartms(t).find(".modal-prompt-input").val())}}]})},e.modalLogin=function(t,n,i,o){return"function"==typeof n&&(o=arguments[2],i=arguments[1],n=void 0),e.modal({text:t||"",title:void 0===n?e.params.modalTitle:n,afterText:'<div class="input-field modal-input-double"><input type="text" name="modal-username" placeholder="'+e.params.modalUsernamePlaceholder+'" class="modal-text-input"></div><div class="input-field modal-input-double"><input type="password" name="modal-password" placeholder="'+e.params.modalPasswordPlaceholder+'" class="modal-text-input"></div>',buttons:[{text:e.params.modalButtonCancel,onClick:function(t){o&&o(smartms(t).find('.modal-text-input[name="modal-username"]').val(),smartms(t).find('.modal-text-input[name="modal-password"]').val())}},{text:e.params.modalButtonOk,bold:!0,onClick:function(t){i&&i(smartms(t).find('.modal-text-input[name="modal-username"]').val(),smartms(t).find('.modal-text-input[name="modal-password"]').val())}}]})},e.modalPassword=function(t,n,i,o){return"function"==typeof n&&(o=arguments[2],i=arguments[1],n=void 0),e.modal({text:t||"",title:void 0===n?e.params.modalTitle:n,afterText:'<div class="input-field"><input type="password" name="modal-password" placeholder="'+e.params.modalPasswordPlaceholder+'" class="modal-text-input"></div>',buttons:[{text:e.params.modalButtonCancel,onClick:function(t){o&&o(smartms(t).find('.modal-text-input[name="modal-password"]').val())}},{text:e.params.modalButtonOk,bold:!0,onClick:function(t){i&&i(smartms(t).find('.modal-text-input[name="modal-password"]').val())}}]})},e.showPreloader=function(t){return e.modal({title:t||e.params.modalPreloaderTitle,text:'<div class="preloader">'+(e.params.material?e.params.materialPreloaderHtml:"")+"</div>",cssClass:"modal-preloader"})},e.hidePreloader=function(){e.closeModal()},e.showIndicator=function(){smartms("body").append('<div class="preloader-indicator-overlay"></div><div class="preloader-indicator-modal"><span class="preloader preloader-white"></span></div>')},e.hideIndicator=function(){smartms(".preloader-indicator-overlay, .preloader-indicator-modal").remove()},e.actions=function(t){(t=t||[]).length>0&&!smartms.isArray(t[0])&&(t=[t]);for(var n=e.params.modalActionsTemplate,i="",o=0;o<t.length;o++)for(var r=0;r<t[o].length;r++){0===r&&(i+='<div class="actions-modal-group">');var s=t[o][r],a="actions-modal-button";s.bold&&(a+=" actions-modal-button-bold"),s.red&&(a+=" actions-modal-button-red"),i+='<span class="'+a+'">'+s.text+"</span>",r===t[o].length-1&&(i+="</div>")}var l=n.replace(/{{buttons}}/g,i);e._modalTemlateTempDiv.innerHTML=l;var u=smartms(e._modalTemlateTempDiv).children();return smartms("body").append(u[0]),u.find(".actions-modal-group").each(function(n,i){var o=n;smartms(i).find(".actions-modal-button").each(function(n,i){var r=n,s=t[o][r];smartms(i).on("click",function(t){!1!==s.close&&e.closeModal(u),s.onClick&&s.onClick(u,t)})})}),e.openModal(u),u[0]},e.openModal=function(t){if(t=smartms(t),0===smartms(".modal-overlay").length){var e=document.createElement("div");e.className="modal-overlay",smartms("body").append(e)}t.hasClass("actions-modal")||t.css({marginTop:-t.outerHeight()/2+"px"});t[0].clientLeft;return smartms(".modal-overlay").addClass("modal-overlay-visible"),smartms(t).addClass("modal-in"),!0},e.closeModal=function(t){return t=smartms(t||".modal-in"),smartms(".modal-overlay").removeClass("modal-overlay-visible"),t.trigger("close"),t.toggleClass("modal-in modal-out").transitionEnd(function(e){t.trigger("closed"),t.remove()}),!0}};