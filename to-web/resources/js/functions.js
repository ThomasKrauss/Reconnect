var GlobalEvents, EndPoints;

var isArray = function(obj) {
	 return Object.prototype.toString.call(obj) === '[object Array]';
};

var isFunction = function(obj) {
	return typeof obj === 'function';
};

/*
	Deals with array and object, even when empty.
	The signature of the given function for an array is (value, index) and for an object (key, value).
*/
var myMap = function(obj, fn) {
	var result = [], keyOrIndex, arrayConfirmed;
	
	if ( 'function' !== typeof fn ) {
		throw new TypeError( fn + ' is not a function' );
	}
	
	arrayConfirmed = isArray(obj);
	if(arrayConfirmed || $.isPlainObject(obj)) {
		if(arrayConfirmed) {
			for(keyOrIndex=0; keyOrIndex<obj.length; keyOrIndex++) {
				result.push(fn.apply(null, [obj[keyOrIndex], keyOrIndex]));
			}
		} else if(obj) {
			for(keyOrIndex in obj) {
				if(obj.hasOwnProperty(keyOrIndex)) {
					result.push(fn.apply(null, [keyOrIndex, obj[keyOrIndex]]));
				}
			}
		}
	}
	
	return result;
};

/*
	Deals with array and object, even when empty.
	With no argument, the given function must return the desired neutral element that makes sense with it (0 for the addition, [] for concatenation of array, etc).
*/
var myReduce = function(obj, fn) {
	var result, i, internalArray, keyOrIndex, arrayConfirmed;
	
	if ( 'function' !== typeof fn ) {
		throw new TypeError( fn + ' is not a function' );
	}
	
	result = fn.apply();
	arrayConfirmed = isArray(obj);
	
	if(arrayConfirmed || $.isPlainObject(obj)) {
		if(arrayConfirmed) {
			for(keyOrIndex=0; keyOrIndex<obj.length; keyOrIndex++) {
				result = fn.apply(null, [result, obj[keyOrIndex]]);
			}
		} else {
			for(keyOrIndex in obj) {
				if(obj.hasOwnProperty(keyOrIndex)) {
					result = fn.apply(null, [result, obj[keyOrIndex]]);
				}
			}
		}
	}
	
	return result;
};

function clone(obj) {
    // Handle the 3 simple types, and null or undefined
    if (null == obj || "object" != typeof obj) return obj;

    // Handle Date
    if (obj instanceof Date) {
        var copy = new Date();
        copy.setTime(obj.getTime());
        return copy;
    }

    // Handle Array
    if (obj instanceof Array) {
        var copy = [];
        for (var i = 0, len = obj.length; i < len; i++) {
            copy[i] = clone(obj[i]);
        }
        return copy;
    }

    // Handle Object
    if (obj instanceof Object) {
        var copy = {};
        for (var attr in obj) {
            if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
        }
        return copy;
    }

    throw new Error("Unable to copy obj! Its type isn't supported.");
}

function empty(domElement) {
	while (domElement.firstChild) {
		domElement.removeChild(domElement.firstChild);
	}
}

function makeEventManager() {
	var eventManager, listenersStore;
	
	/*
		Fire the event of the given name, passing to each listener the <eventArguments>.
		
		The difference with registerAsEmitter is that listeners don't have anything bound to the 'this' keyword.
		As such, this function is useful for global commands (such as the ones resulting from user interaction).
		But it's not really appropriate for inter-components communication.
	*/
	var fireEvent = function(eventName, eventArguments) {
		var listeners = listenersStore[eventName], args = eventArguments ? eventArguments : [];
		myMap(listeners, function(cb) {
			cb.apply(null, args);
		});
	};
	
	/*
		Listen to the event of given name with the given function.
	*/
	var listenToEvent = function(eventName, cb) {
		if(!listenersStore[eventName]) {
			listenersStore[eventName] = [];
		}
		
		if(isFunction(cb)) {
			listenersStore[eventName].push(cb);
		} else {
			console.error('Try to listen to global event '+eventName+' with a '+(typeof cb)+' instead of a function!');
		}
	};
	
	var registerAsEmitter = function(obj, eventName) {
		var objListenersStore = [], onEventName = 'on' + eventName.charAt(0).toUpperCase() + eventName.slice(1);
		
		// for listening to the event
		obj[onEventName] = function(cb) {
			if(isFunction(cb)) {
				objListenersStore.push(cb);
			} else {
				console.error('Try to listen to event '+eventName+' with a '+(typeof cb)+' instead of a function!');
			}
		};
		
		// for firing the event
		obj[eventName] = function() {
			var listeners = objListenersStore, eventArguments = arguments;
			myMap(listeners, function(cb) {
				cb.apply(obj, eventArguments);
			});
		};
	};
	
	var reset = function() {
		eventManager = {};
		listenersStore = {};
		
		eventManager.registerAsEmitter = registerAsEmitter;
		eventManager.listenToEvent = listenToEvent;
		eventManager.fireEvent = fireEvent;
	};
	reset();
	return eventManager;
}

/*
	Requesting resources.
	- /foo/bar (or /foo/bar/): obtaining the precise resource bar in foo
	- /foo/bar/* : obtaining everything starting at resource bar in foo (if it exists) and everything that is in bar
	- /foo/bar/<nb> : starting at resource bar in foo, get everything up to <nb> levels in bar. If 0, it is equivalent to listening to the precise resource bar in foo. If negative, nothing is being listened to.
	- /foo/bar/<nb>-<nb> : a range, starting at the depth in bar indicated by the first number, going to the depth indicated by the second number, including the resources obtainable at that last level. When the first number is greater than the second, nothing is being listened to.
*/
var makeEndPoints = function (handlePageChangeEvent, getWatcherUrls) {
    var endPoints;
	var watcherUrls;
    var socket;
	var _handlePageChangeEvent = handlePageChangeEvent !== undefined ? handlePageChangeEvent : true;
	var _getWatcherUrls = getWatcherUrls !== undefined ? getWatcherUrls : false;
	var SHDrangeRegexp = /^(\d+)-(\d+)$/;
	
	//SHD = shall handle data
	
    var makeDefaultSHD = function (expectedKeys) {
		return function(dataKeys) {
			var result;
			var i = 0;
			result = expectedKeys.length === dataKeys.length;
			while (result && i < expectedKeys.length) {
				result = expectedKeys[i] === dataKeys[i];
				++i;
			};
			return result;
		};
    };
	var makeSHDfromDepth = function(expectedKeys) {
		return function(dataKeys) {
			var result;
			var i = 0;
			result = expectedKeys.length <= dataKeys.length;
			while (result && i < expectedKeys.length) {
				result = expectedKeys[i] === dataKeys[i];
				++i;
			};
			return result;
		};
	};
	var makeSHDfromDepthToDepth = function(expectedKeys, fromDepth, toDepth) {
		return function(dataKeys) {
			var result;
			var i = 0;
			result = fromDepth <= dataKeys.length && dataKeys.length <= toDepth;
			while (result && i < expectedKeys.length) {
				result = expectedKeys[i] === dataKeys[i];
				++i;
			};
			return result;
		};
	};
    var makeListener = function (eventName, callback, shallHandleData) {
        return function (jsonString) {
            var data;
            try {
                data = JSON.parse(jsonString);
            } catch (e) {
                console.error('Cannot parse JSON on socket communication of event ' + eventName + '. Error is ' + e);
            };
			return myMap(data, function (item) {
				if(shallHandleData.apply(null, [item.keys])) {
					callback.apply(null, [item]);
				}
			});
        };
    };
    var on = function (name, cb) {
        var eventName;
        var keys;
		var lastKey;
		var handleDataFn;
		var range, depthLimit, ignoreLimit;
        if (isFunction(cb)) {
            keys = name.split('/');
            eventName = keys.splice(0, 1)[0];
			lastKey = keys[keys.length - 1];
			
			if(lastKey === '*') {
				keys.pop();
				handleDataFn = makeSHDfromDepth(keys);
				depthLimit = false;
				
			} else if(lastKey && !isNaN(lastKey)) {
				depthLimit = lastKey * 1;
				keys.pop();
				if(depthLimit > 0) {
					handleDataFn = makeSHDfromDepthToDepth(keys, keys.length, keys.length + depthLimit);
				} else if(depthLimit === 0) {
					handleDataFn = makeDefaultSHD(keys);
				}
				
			} else if(SHDrangeRegexp.test(lastKey)) {
				keys.pop();
				range = lastKey.split('-');
				depthLimit = range[1]*1;
				ignoreLimit = range[0]*1;
				if(ignoreLimit <= depthLimit) {
					handleDataFn = makeSHDfromDepthToDepth(keys, keys.length + ignoreLimit, keys.length + depthLimit);
				}
				
			} else {
				if(lastKey === '') {
					keys.pop();
				}
				handleDataFn = makeDefaultSHD(keys);
			}
			
			if(handleDataFn) {
				name = eventName;
				if(keys.length > 0) {
					name += '/' + keys.join('/');
				}
				
				socket.on(eventName, makeListener(eventName, cb, handleDataFn));
				socket.on('no-' + eventName, makeListener('no-' + eventName, cb, handleDataFn));
				
				getResources(name, depthLimit, ignoreLimit-1, function (data) {
					return myMap(data, cb);
				});
			}
        } else {
            return console.error('Cannot setup listening to resource ' + name + ': the callback is actually not a function!');
        };
    };
	var getResources = function(name, depthLimit, ignoreLimit, callback) {
		$.ajax({ 'url' : watcherUrls.rest + '/get-resources',
								'dataType' : 'jsonp',
								'data' : { 'name' : name, 'depthLimit': depthLimit, 'ignoreLimit': ignoreLimit },
								'success' : callback,
								'error' : function (request, status, error) {
					return console.error('Cannot initialize data for resource ' + name + '. Status is '+status+' Error is' + error);
				}
                });
	};
	var onAction = function(actionName, callback) {
		socket.on(actionName, function(jsonString) {
			var data;
            try {
                data = JSON.parse(jsonString);
            } catch (e) {
                console.error('Cannot parse JSON on socket communication of action ' + actionName + '. Error is ' + e);
            };
            if (data) {
				callback.apply(null, [data]);
            };
		});
	};
	var notifyWatcher = function(actionName, params) {
		var data = {'name': actionName};
		myMap(params, function(key, value) {
			data[key] = value;
		});
		$.ajax({ 'url' : watcherUrls.rest + '/act',
								'dataType' : 'jsonp',
								'data' : data,
								'error' : function (request, status, error) {
					return console.error('Cannot initialize data for resource ' + name + '. Status is '+status+' Error is' + error);
				}
                });
	};
	var scrollToPosition = function (position) {
        return position && position[1] && position[2] ? window.scrollTo(parseInt(position[1]), parseInt(position[2])) : null;
    };
    var handlePageChange = function (url) {
        var x = null;
        var y = null;
        var fullUrl = JSON.parse(url).value;
        var extension = fullUrl.slice(1 + fullUrl.lastIndexOf('.'));
        if (extension === 'html') {
            if (window.location.href.indexOf(fullUrl) !== -1) {
                x = document.pageXOffset ? document.pageXOffset : document.body.scrollLeft;
                y = document.pageYOffset ? document.pageYOffset : document.body.scrollTop;
                return window.location.search = '?x=' + x + '&y=' + y;
            } else {
                return window.location.pathname = fullUrl;
            };
        } else if (extension === 'css' || extension === 'js') {
            return window.location.reload(true);
        };
    };
    var reset = function () {
        var previousPosition;
        endPoints = {  };
		serverUrl = window.location.protocol + '//' + window.location.host;
		watcherUrls = {'rest': serverUrl};
		if(_getWatcherUrls) {
			$.ajax({ 'url' : serverUrl + '/list-watcher-urls',
					'async' : false,
					'type' : 'POST',
					'success' : function (data) {
						try {
							watcherUrls = JSON.parse(data);
						} catch(e) {
							console.error('JSON parse error during ajax communication /list-watcher-urls : ' + error);
							console.error('ajax communication /list-watcher-urls response content is [' + str + ']');
						}
				}});
		}
		socket = io.connect(watcherUrls.rest);
		if(_handlePageChangeEvent) {
			socket.on('pageHasChanged', handlePageChange);
		}
        previousPosition = /\?x=([0-9]*)&y=([0-9]*)/.exec(window.location.search);
        scrollToPosition(previousPosition);
		endPoints.notifyWatcher = notifyWatcher;
		endPoints.onAction = onAction;
		endPoints.getResources = getResources;
        return endPoints.on = on;
    };
    reset();
    return endPoints;
};

var matchSymbolParts = function(option, queryParts) {
	var optionParts = option.split('-'), result = (optionParts.length >= queryParts.length);
	var a, b, i = 0;
	
	while(result && i<queryParts.length) {
		a = optionParts[i];
		b = queryParts[i];
		
		result = (a.slice(0, Math.min(a.length, b.length)) === b);
		i++;
	}
	
	return result;
};

var getMatchingSymbols = function(query, lst, key) {
	var queryParts, option, result = [], match;
	
	if(lst && lst.length > 0) {
		queryParts = query ? query.split('-') : [];
		
		for(var i=0; i<lst.length; i++) {
			option = lst[i];
			match = matchSymbolParts(isFunction(key) ? key(option) : option, queryParts);
			if(match) {
				result.push(option);
			}
		}
	}
	
	return result;
};

/**
* Global key listener
* Essentially made from CodeMirror code, by Marijn Haverbeke.
*/
function makeGlobalKeyBinder(commands, keyMap, initKeyMap) {
	var globalKeyBinder;
	var maybeAutoTransition, maybeBailOutTransition;
	var i, currentKeyMap;
	
	/** Methods **/
	var setCurrentKeyMap = function(name) {
		var km = globalKeyBinder.keyMap[name], bailOutNext, startMap;
		if(km) {
			currentKeyMap = name;
			
			clearTimeout(maybeBailOutTransition);
			startMap = getKeyMap(name);
			bailOutNext = getKeyMap(name).bailOut;
			if (bailOutNext) maybeBailOutTransition = setTimeout(function() {
				if (getKeyMap(currentKeyMap) == startMap) {
					if(isFunction(bailOutNext)) {
						globalKeyBinder.setCurrentKeyMap(bailOutNext.apply(null, []));
					} else {
						globalKeyBinder.setCurrentKeyMap(bailOutNext);
					}
				}
			}, 2500);
		} else {
			console.error('globalKeyBinder.setCurrentKeyMap: no keymap named ['+name+']');
		}
	};

	var getCurrentKeyMap = function() {
		return currentKeyMap;
	};
	
	/** Internals **/
	// Navigator & OS flags
	var ie = /MSIE \d/.test(navigator.userAgent);
	var opera = /Opera\//.test(navigator.userAgent);
	var ios = /AppleWebKit/.test(navigator.userAgent) && /Mobile\/\w+/.test(navigator.userAgent);
	var mac = ios || /Mac/.test(navigator.platform);
	
	// Key names
	var keyNames = {3: "Enter", 8: "Backspace", 9: "Tab", 13: "Enter", 16: "Shift", 17: "Ctrl", 18: "Alt",
				  19: "Pause", 20: "CapsLock", 27: "Esc", 32: "Space", 33: "PageUp", 34: "PageDown",
				  35: "End", 36: "Home", 37: "Left", 38: "Up", 39: "Right", 40: "Down", 44: "PrintScrn",
				  45: "Insert", 46: "Delete", 59: ";", 61: "=", 91: "Mod", 92: "Mod", 93: "Mod", 106: "*", 107: "+",
				  109: "-", 111: "/", 127: "Delete", 169: ")", 186: ";", 187: "=", 188: ",", 189: "-", 190: ".",
				  191: "/", 192: "`", 219: "[", 220: "\\", 221: "]", 222: "'", 63276: "PageUp",
				  63277: "PageDown", 63275: "End", 63273: "Home", 63234: "Left", 63232: "Up",
				  63235: "Right", 63233: "Down", 63302: "Insert", 63272: "Delete"};
	// Number keys
	for (i = 0; i < 10; i++) { keyNames[i + 48] = keyNames[i + 96] = String(i); }
	// Alphabetic keys
	for (i = 65; i <= 90; i++) { keyNames[i] = String.fromCharCode(i); }
	// Function keys
	for (i = 1; i <= 12; i++) { keyNames[i + 111] = keyNames[i + 63235] = "F" + i; }
	
	/** Events utility **/
	// Allow 3rd-party code to override event properties by adding an override
	// object to an event object.
	var e_prop = function(e, prop) {
		var overridden = e.override && e.override.hasOwnProperty(prop);
		return overridden ? e.override[prop] : e[prop];
	};
	
	var stopMethod = function() {e_stop(this);}
	// Ensure an event has a stop method.
	var addStop = function(event) {
		if (!event.stop) { event.stop = stopMethod; }
		return event;
	};
	
	var e_preventDefault = function(e) {
		if (e.preventDefault) e.preventDefault();
		else e.returnValue = false;
	};
	var e_stopPropagation = function(e) {
		if (e.stopPropagation) e.stopPropagation();
		else e.cancelBubble = true;
	}
	var e_stop = function(e) {e_preventDefault(e); e_stopPropagation(e);}
	
	
	/** Key-related utilities **/
	var getKeyMap = function(val) {
		if (typeof val == "string") return globalKeyBinder.keyMap[val];
		else return val;
	}
	var lookupKey = function(name, map, handle, stop) {
		var tmp;
		function lookup(map) {
			map = getKeyMap(map);
			var found = map[name];
			if (found === false) {
				if (stop) stop();
				return true;
			}
			if (found != null && handle(found)) {
				return true;
			}
			if (map.nofallthrough) {
				if (stop) stop();
				return true;
			}
			var fallthrough = map.fallthrough;
			if (fallthrough == null) return false;
			if (!isArray(fallthrough)) {
				return lookup(fallthrough);
			}
			for (var i = 0, e = fallthrough.length; i < e; ++i) {
				if (lookup(fallthrough[i])) return true;
			}
			return false;
		}
		return lookup(map);
	}
	var isModifierKey = function(event) {
		var name = keyNames[event.keyCode];
		return name == "Ctrl" || name == "Alt" || name == "Shift" || name == "Mod";
	}
	
	var Pass = {toString: function(){return "keybinder.Pass";}};
	var doHandleBinding = function(bound, event) {
		if (typeof bound == "string") {
			bound = globalKeyBinder.commands[bound];
			if (!bound) return false;
		}
		try {
			bound(event);
		} catch(e) {
			if (e != Pass) throw e;
			return false;
		}
		return true;
	}
	// Is key handled?
	var handleKeyBinding = function(e) {
		// Handle auto keymap transitions
		var startMap = getKeyMap(currentKeyMap), autoNext = startMap.auto, bailOutNext = startMap.bailOut;
		clearTimeout(maybeAutoTransition);
		if (autoNext && !isModifierKey(e)) {
			maybeAutoTransition = setTimeout(function() {
				if (getKeyMap(currentKeyMap) == startMap) {
					if(isFunction(autoNext)) {
						globalKeyBinder.setCurrentKeyMap(autoNext.apply(null, []));
					} else {
						globalKeyBinder.setCurrentKeyMap(autoNext);
					}
				}
			}, 50);
		}
		
		clearTimeout(maybeBailOutTransition);
		if (bailOutNext && !isModifierKey(e)) {
			maybeBailOutTransition = setTimeout(function() {
				if (getKeyMap(currentKeyMap) == startMap) {
					if(isFunction(bailOutNext)) {
						globalKeyBinder.setCurrentKeyMap(bailOutNext.apply(null, []));
					} else {
						globalKeyBinder.setCurrentKeyMap(bailOutNext);
					}
				}
			}, 2500);
		}
		
		// Compute the keyname (in CM, that was done except for shift key press which requires
		// special handling because "Shift"+name may really be a meaningful combination of keys
		// or it could be that shift was used for selecting text regions)
		var name = keyNames[e_prop(e, "keyCode")], handled = false;
		var flipCtrlCmd = opera && mac;
		if (name == null || e.altGraphKey) { return true; }
		if (e_prop(e, "altKey")) { name = "Alt-" + name; }
		if (e_prop(e, flipCtrlCmd ? "metaKey" : "ctrlKey")) { name = "Ctrl-" + name; }
		// don't know why metaKey and ctrlKey are true at the same time
		//if (e_prop(e, flipCtrlCmd ? "ctrlKey" : "metaKey")) { name = "Cmd-" + name; }
		if (e_prop(e, "shiftKey")) { name = "Shift-" + name; }
		
		// Determine if it's handled
		var stopped = false;
		function stop() { stopped = true; }
		
		handled = lookupKey(name, currentKeyMap, function (b) { return doHandleBinding(b,e); }, stop);
		
		if (stopped) handled = false;
		if (handled) {
			e_preventDefault(e);
			if (ie) { e.oldKeyCode = e.keyCode; e.keyCode = 0; }
		}
		
		return !handled;
	}
	
	var reset = function() {
		$(document).unbind('keydown', handleKeyBinding);
		
		globalKeyBinder = {'commands': commands, 'keyMap': keyMap, 'keyNames': keyNames};
		currentKeyMap = initKeyMap;
		
		globalKeyBinder.setCurrentKeyMap = setCurrentKeyMap;
		globalKeyBinder.getCurrentKeyMap = getCurrentKeyMap;
		
		$(document).keydown(handleKeyBinding);
	};
	reset();
	return globalKeyBinder;
}

function dashboardNavigate(names) {
	var result = window.location.protocol + '//' + window.location.host + '/';
	
	switch(names.length) {
		case 1:
			result += 'system.html?system-name='+names[0];
			break;
		
		case 2:
			result += 'module.html?system-name='+names[0]+'&module-name='+names[1];
			break;
			
		default:
			console.error('Cannot handle a depth of '+names.length+' for names '+JSON.stringify(names));
			break;
	}
	
	return result;
}