/** @jsx React.DOM */
GlobalEvents = makeEventManager();
EndPoints = makeEndPoints(false, true);

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../lib/codemirror"), require("../addon/search/searchcursor"), require("../addon/edit/matchbrackets"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../lib/codemirror", "../addon/search/searchcursor", "../addon/edit/matchbrackets"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
	"use strict";
	
	var dictionnary = {};
	var updateDictionnary = function(data) {
		var entry = data.value;
		dictionnary[entry.name] = entry;
	};
	EndPoints.on('package-symbols/1', updateDictionnary);
	
	var getAllMatchingSymbols = function(symbol, systemName) {
		var entry, result = [];
		
		entry = dictionnary[systemName];
		if(entry) {
			result = result.concat(getMatchingSymbols(symbol, entry.internal));
			result = result.concat(getMatchingSymbols(symbol, entry.external));
			result = result.concat(getMatchingSymbols(symbol, entry.inherited));
		}
		
		return result.sort();
	};
	
	var getInternalMatchingSymbols = function(symbol, systemName) {
		var entry, result = [];
		
		entry = dictionnary[systemName];
		if(entry) {
			result = myMap(getMatchingSymbols(symbol, entry.internal), function(symbol) {
				return systemName + '::' + symbol;
			});
		}
		
		return result.sort();
	}; 
	
	var getExternalMatchingSymbols = function(symbol, systemName) {
		var entry, result = [];
		
		entry = dictionnary[systemName];
		if(entry) {
			var tmp = getMatchingSymbols(symbol, entry.external);
			result = myMap(tmp, function(symbol) {
				return systemName + ':' + symbol;
			});
		}
		
		return result.sort();
	}; 
	
	var getInheritedMatchingSymbols = function(symbol, systemName) {
		var entry, result = [];
		
		entry = dictionnary[systemName];
		if(entry) {
			result = getMatchingSymbols(symbol, entry.inherited);
		}
		
		return result.sort();
	}; 
	
	var map = CodeMirror.keyMap.reconnect = {'fallthrough': 'default'};
	
	map['Tab'] = function(editor) {
		var autocompleteShown = false;
		
		editor.showHint({'hint': function(editor) {
			var cursorPosition, token, symbol, matchingSymbols = [];
			var position, lastPosition, prefix, internal;
			var result;
			
			cursorPosition = editor.getCursor();
			token = editor.getTokenAt(cursorPosition);
			
			position = token.string.indexOf(':');
			if(position > -1) {
				prefix = token.string.slice(0, position);
				lastPosition = token.string.lastIndexOf(':');
				
				if(lastPosition === position + 1) {
					internal = true;
					symbol = token.string.slice(lastPosition+1);
				} else if(lastPosition === position) {
					symbol = token.string.slice(lastPosition+1);
				} else {
					console.error('A symbol should contain only one colon or two colons in a row. Autocompletion aborted on '+token.string);
				}
			} else {
				symbol = token.string;
			}
			
			if(symbol && (token.type === 'keyword' || token.type === 'atom' || token.type === 'variable' || token.type === 'variable-2')) {
				if(prefix !== undefined) {
					if(prefix === '') {
						matchingSymbols = myMap(getAllMatchingSymbols(symbol, 'keyword'), function(symbol) {
							return ':' + symbol;
						});
					} else if(internal) {
						matchingSymbols = getInternalMatchingSymbols(symbol, prefix);
						matchingSymbols = matchingSymbols.concat(getInheritedMatchingSymbols(symbol, editor.getEditedSystemName()));
					} else {
						matchingSymbols = getExternalMatchingSymbols(symbol, prefix);
					}
				} else {
					matchingSymbols = getAllMatchingSymbols(symbol, editor.getEditedSystemName());
				}
				
				result = {'from': CodeMirror.Pos(cursorPosition.line, token.start),
						'to': CodeMirror.Pos(cursorPosition.line, token.end),
						'list': matchingSymbols};
				
				CodeMirror.on(result, 'shown', function() {
					autocompleteShown = true;
				});
				
				CodeMirror.on(result, 'close', function() {
					autocompleteShown = false;
				});
			}
			
			if(!autocompleteShown && matchingSymbols.length ===0) {
				editor.indentLine(cursorPosition.line);
			}
			
			return result;
		},
		'customKeys': {
			  Up: function(editor, handle) {handle.moveFocus(-1);},
			  Down: function(editor, handle) {handle.moveFocus(1);},
			  PageUp: function(editor, handle) {handle.moveFocus(-handle.menuSize() + 1, true);},
			  PageDown: function(editor, handle) {handle.moveFocus(handle.menuSize() - 1, true);},
			  Home: function(editor, handle) {handle.setFocus(0);},
			  End: function(editor, handle) {handle.setFocus(handle.length - 1);},
			  Enter: function(editor, handle) { handle.pick();},
			  Tab: function(editor, handle) {handle.moveFocus(1);},
			  Esc: function(editor, handle) { handle.close();}
		}});
		
		return false;
	};
	
	map['Alt-Up'] = function(editor) {
		var linePosition, isEmpty = false, emptyLineRegex = /^\s*$/;
		
		if(isFunction(editor.ctrlUp)) {
			editor.ctrlUp();
		} else {
			linePosition = editor.getCursor().line - 1;
			
			// if we start on a blank line, ignore all successive blank lines
			// We then reach the end of some content
			if(linePosition > 0) {
				isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
				while(linePosition > 0 && isEmpty) {
					linePosition--;
					isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
				}
			}
			
			// Get to the start of the content
			while(linePosition > 0 && !isEmpty) {
				linePosition--;
				isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
			}
			
			// Move cursor if we are on an empty line or at the start of the document
			if(isEmpty || linePosition === 0) {
				// If we are on an empty line, move on to the next (which necessarily hold some content)
				if(linePosition > 0) {
					linePosition++;
				}
				editor.setCursor({'line': linePosition, 'ch': 0});
			}
		}
	};
	
	map['Alt-Down'] = function(editor) {
		var linePosition, totalLines, isEmpty = false, emptyLineRegex = /^\s*$/;
		
		if(isFunction(editor.ctrlDown)) {
			editor.ctrlDown();
		} else {
			totalLines = editor.lineCount() - 1;
			linePosition = editor.getCursor().line;
			
			// if we do not start on some content, ignore the content
			isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
			while(linePosition < totalLines && !isEmpty) {
				linePosition++;
				isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
			}
			
			// ignore all successive blank lines to reach the next content
			while(linePosition < totalLines && isEmpty) {
				linePosition++;
				isEmpty = emptyLineRegex.test(editor.getLine(linePosition));
			}
			
			if(!isEmpty && linePosition < totalLines) {
				editor.setCursor({'line': linePosition, 'ch': 0});
			}
		}
	};
	
	var scanForEditedChunk = function(editor) {
		var totalLines, isEmpty, emptyLineRegex = /^\s*$/;
		var cursorLinePosition, startChunkPosition, endChunkPosition, chunks, chunk;
		
		cursorLinePosition = editor.getCursor().line;
		if(!emptyLineRegex.test(editor.getLine(cursorLinePosition))) {
			
			isEmpty = false;
			startChunkPosition = cursorLinePosition;
			while(startChunkPosition > 0 && !isEmpty) {
				startChunkPosition--;
				isEmpty = emptyLineRegex.test(editor.getLine(startChunkPosition));
			}
			
			if(startChunkPosition > 0 && isEmpty) {
				startChunkPosition++;
			}
			
			totalLines = editor.lineCount() - 1;
			isEmpty = false;
			endChunkPosition = cursorLinePosition;
			while(endChunkPosition < totalLines && !isEmpty) {
				endChunkPosition++;
				isEmpty = emptyLineRegex.test(editor.getLine(endChunkPosition));
			}
			
			if(endChunkPosition < totalLines && isEmpty) {
				endChunkPosition--;
			}
			
			chunks = editor.getChunks({'line': startChunkPosition, 'ch': 0},
									{'line': endChunkPosition, 'ch': editor.getLine(endChunkPosition).length});
			
			if(chunks && chunks.length > 0) {
				chunk = chunks[0];
			}
		}
		
		return chunk;
	};
	
	map['Alt-Left'] = function(editor) {
		editor.retract(scanForEditedChunk(editor));
	};
	
	map['Alt-Right'] = function(editor) {
		editor.expand(scanForEditedChunk(editor));
	};
});

$(document).ready(function() {
	var moduleName, systemName;
	
	// the specific verbs of code edition
	var getResourceMessages, refreshCompileProblems, refreshUsagesProblems;
	var loadCode, saveCode, selectCode;
	var displayMessages, refreshMessages, clearMessages;
	var loadUsageCode, saveUsageCode, selectUsageCode;
	var hideCodeEditors, showCodeEditors, hideUsageEditors, showUsageEditors;
	var focusCodeEditor, focusUsageEditor;
	var setMaintenanceMode, setInCodeEditionMode, setInUsageEditionMode, setTaskEditionMode;
	var save, guardedSave;
	var expandToUsageView;
	
	// the specific context
	var focusedActionName, selectedModuleName, selectedSystemName, filtering = false, currentMessages = [], editingTest, inMaintenanceMode = false;
	
	// the component verbs
	var commandFocus;
	var activateTopMaintenanceTask, deactivateTopMaintenanceTask;
	var codeEditorLoad, codeEditorSave, codeEditorFocus;
	var codeChunkEditorLoad, codeChunkEditorSave, codeChunkEditorFilter, codeChunkEditorFocus;
	var usageEditorLoad, usageEditorSave, usageEditorFocus;
	var usageChunkEditorLoad, usageChunkEditorSave, usageChunkEditorFocus;
	
	getResourceMessages = function(endPointName, entry, callback) {
		var messages = [], exclude = {'usages':['ok']};
		var isDataExcluded = function(endPointName, dataKey) {
			return isArray(exclude[endPointName]) && exclude[endPointName].indexOf(dataKey) !== -1;
		};
		
		EndPoints.getResources([endPointName].concat(entry).join('/'), 1, 0, function(data) {
			myMap(data, function(item) {
				var inProblem = false, entry;
				
				myMap(item.value && item.value.stats, function(key, value) {
					inProblem = inProblem || (!isDataExcluded(endPointName, key) && value > 0);
				});
				
				if(inProblem) {
					entry = {'id': item.value.name};
					
					myMap(item.value.messages, function(key, value) {
						if(value) {
							entry[key] = value;
						}
					});
					
					messages.push(entry);
				}
			});
			
			callback(messages);
		});
	};
	
	refreshCompileProblems = function() {
		getResourceMessages('compile-problems', [selectedSystemName, selectedModuleName], refreshMessages);
	};
	
	refreshUsagesProblems = function() {
		getResourceMessages('usage', [selectedSystemName, selectedModuleName, focusedActionName], refreshMessages);
	};
	
	loadCode = function(moduleName, systemName) {
		var result;
		
		selectedModuleName = moduleName;
		selectedSystemName = systemName;
		focusedActionName = null;
		
		result = loadChunks(moduleName, systemName);
		clearMessages();
		refreshCompileProblems();
		
		return result;
	};
	
	saveCode = function(replacements, deletions) {
		return saveChunks(JSON.stringify(replacements), JSON.stringify(deletions));
	};
	
	selectCode = function(moduleName, systemName, ids) {
		if(inMaintenanceMode && selectedModuleName === moduleName && selectedSystemName === systemName) {
			if(filtering) {
				codeChunkEditorFilter([]);
			} else {
				codeChunkEditorFilter(ids);
			}
			filtering = !filtering;
		} else {
			if(inMaintenanceMode) {
				codeChunkEditorLoad(moduleName, systemName, ids);
			} else {
				codeEditorLoad(moduleName, systemName, ids);
			}
			currentMessages = [];
			filtering = isArray(ids) && ids.length > 0;
			
			EndPoints.notifyWatcher('focus', {'systemName': systemName, 'moduleName': moduleName});
		}
	};
	
	displayMessages = function(messages) {
		if(messages) {
			currentMessages = messages;
		}
		React.renderComponent(
		  <MessageBox messages={currentMessages} />,
		  document.getElementById('messageBox')
		);
	};
	
	refreshMessages = function(messages) {
		var replacedIds, mergedMessages, toAppend;
		var find = function(id, array) {
			var i = 0, element;
			while(!element && i < array.length) {
				if(array[i].id === id) {
					element = array[i];
				}
				i++;
			}
			
			return element;
		};
		
		mergedMessages = myMap(currentMessages, function(value) {
			var found = find(value.id, messages);
			return found ? found : value;
		});
		
		toAppend = [];
		myMap(messages, function(value) {
			if(!find(value.id, mergedMessages)) {
				toAppend.push(value);
			}
		});
		
		displayMessages(mergedMessages.concat(toAppend));
	};
	
	clearMessages = function() {
		displayMessages([]);
	};
	
	loadUsageCode = function(moduleName, systemName, actionName) {
		var result;
		if(moduleName && systemName && actionName) {
			focusedActionName = actionName;
			result = loadUsageChunks(actionName, moduleName, systemName);
			// refreshUsagesProblems();
		}
		return result;
	};
	
	saveUsageCode = function(replacements, deletions) {
		return saveUsageChunks(JSON.stringify(replacements), JSON.stringify(deletions));
	};
	
	hideCodeEditors = function() {
		if(inMaintenanceMode) {
			$('#codeEditor').hide();
			$('#codeChunkEditor').show().css({'visibility': 'hidden'});
		} else {
			$('#codeEditor').show().css({'visibility': 'hidden'});
			$('#codeChunkEditor').hide();
		}
	};
	
	showCodeEditors = function() {
		if(inMaintenanceMode) {
			$('#codeChunkEditor').css({'visibility': 'visible'});
		} else {
			$('#codeEditor').css({'visibility': 'visible'});
		}
	};
	
	hideUsageEditors = function() {
		$('#usageEditor').hide();
		$('#usageChunkEditor').hide();
	};
	
	showUsageEditors = function() {
		if(inMaintenanceMode) {
			$('#usageEditor').hide();
			$('#usageChunkEditor').show();
		} else {
			$('#usageEditor').show();
			$('#usageChunkEditor').hide();
		}
	};
	
	setMaintenanceMode = function(value) {
		inMaintenanceMode = value;
		selectedSystemName = null;
		selectedModuleName = null;
		focusedActionName = null;
	};
	
	focusCodeEditor = function() {
		if(inMaintenanceMode) {
			codeChunkEditorFocus();
		} else {
			codeEditorFocus();
		}
	};
	
	setInCodeEditionMode = function(moduleName, systemName, ids) {
		var loadAsked = moduleName && systemName;
		
		hideUsageEditors();
		deleteDebuggedUsagesFile();
		
		if(loadAsked) {
			hideCodeEditors();
		}
		
		$('#layout').css({'width': '750px'});
		
		if(loadAsked) {
			selectCode(moduleName, systemName, ids);
			showCodeEditors();
		}
		
		editingTest = false;
		focusCodeEditor();
	};
	
	focusUsageEditor = function() {
		if(inMaintenanceMode) {
			usageChunkEditorFocus();
		} else {
			usageEditorFocus();
		}
	};
	
	selectUsageCode = function(moduleName, systemName, actionName) {
		if(systemName !== selectedSystemName || moduleName !== selectedModuleName || actionName !== focusedActionName) {
			guardedSave(function() {
				makeDebuggedUsagesFile(actionName, moduleName, systemName);
				if(inMaintenanceMode) {
					usageChunkEditorLoad(moduleName, systemName, actionName);
				} else {
					usageEditorLoad(moduleName, systemName, actionName);
				}
				showUsageEditors();
				focusUsageEditor();
			});
		} else {
			makeDebuggedUsagesFile(actionName, moduleName, systemName);
			showUsageEditors();
			focusUsageEditor();
		}
	};
	
	setInUsageEditionMode = function(moduleName, systemName, ids) {
		var loadAsked = moduleName && systemName;
		
		if(loadAsked) {
			hideCodeEditors();
		}
		
		$('#layout').css({'width': '1510px'});
		
		if(loadAsked) {
			showCodeEditors();
			selectUsageCode(moduleName, systemName, ids);
		} else {
			showUsageEditors();
			focusUsageEditor();
		}
		
		editingTest = true;
	};
	
	setTaskEditionMode = function(data) {
		var filteredIds;
		
		if(data) {
			filteredIds = myMap(data.messages, function(value) {
				return value.id;
			});
			
			if(data.maintenanceType) {
				setMaintenanceMode(true);
				setInCodeEditionMode(data.moduleName, data.systemName, filteredIds);
			}
		} else {
			setMaintenanceMode(false);
			setInCodeEditionMode();
		}
	};
	
	save = function() {
		var saveDone = false;
		var messages = [], wipChunkCount = 0, missedChunkCount = 0, deleteCount = 0;
		
		var update = function(data) {
			if(data) {
				messages = messages.concat(data.messages);
				wipChunkCount += data.wipChunkCount;
				missedChunkCount += data.missedChunkCount;
				deleteCount += data.deleteCount;
				
				if((isArray(data.chunks) && data.chunks.length > 0) || data.deleteCount > 0) {
					saveDone = true;
				}
			}
		};
		
		if(inMaintenanceMode) {
			update(codeChunkEditorSave());
		} else {
			update(codeEditorSave());
		}
		refreshCompileProblems();
		
		if(editingTest) {
			if(inMaintenanceMode) {
				update(usageChunkEditorSave());
			} else {
				update(usageEditorSave());
			}
			makeDebuggedUsagesFile(focusedActionName, selectedModuleName, selectedSystemName);
			refreshUsagesProblems();
		}
		
		if(wipChunkCount > 0) {
			messages = messages.concat([{'id': 'Work in progress', 'error': [wipChunkCount+' chunk'+(wipChunkCount === 1 ? ' is' : 's are')+' unfinished. Save aborted.']}]);
		}
		
		if(missedChunkCount > 0) {
			messages = messages.concat([{'id': 'Chunk missed', 'warning': [missedChunkCount+' chunk'+(missedChunkCount === 1 ? ' was' : 's were')+' missed during saving.']}]);
		}
		
		if(messages.length > 0) {
			refreshMessages(messages);
		}
		
		return {'ok': wipChunkCount === 0 && missedChunkCount === 0 && deleteCount === 0 && messages.length === 0,
				'done': saveDone};
	};
	
	guardedSave = function(callback) {
		var timeoutId;
		if(save().ok) {
			timeoutId = window.setTimeout(function() {
				window.clearTimeout(timeoutId);
				clearMessages();
				callback();
			}, 800);
		}
	};
	
	expandToUsageView = function(chunk) {
		if(chunk && chunk.type !== 'fact' && chunk.type !== 'usage') {
			setInUsageEditionMode(chunk.moduleName, chunk.systemName, chunk.id);
		}
	};
	
	$('#layout').append('<div id="codeEditor"></div>');
	$('#layout').append('<div id="usageEditor"></div>');
	$('#layout').append('<div id="codeChunkEditor"></div>');
	$('#layout').append('<div id="usageChunkEditor"></div>');
	
	/** Render editors **/
	makeChunkStreamEditor(document.getElementById('codeEditor'),
		{'load': {'give': loadCode, 'take': function(fn) { codeEditorLoad = fn; }},
		'save': {'give': saveCode, 'take': function(fn) { codeEditorSave = fn; }},
		'focus': {'take': function(fn) { codeEditorFocus = fn; }},
		'onExpand': expandToUsageView,
		'onRetract': setInCodeEditionMode});
	
	makeChunkEditor(document.getElementById('codeChunkEditor'),
		{'load': {'give': loadCode, 'take': function(fn) { codeChunkEditorLoad = fn; }},
		'save': {'give': saveCode, 'take': function(fn) { codeChunkEditorSave = fn; }},
		'filter': {'take': function(fn) { codeChunkEditorFilter = fn; }},
		'focus': {'take': function(fn) { codeChunkEditorFocus = fn; }},
		'onExpand': expandToUsageView});
	
	makeChunkStreamEditor(document.getElementById('usageEditor'),
		{'load': {'give': loadUsageCode, 'take': function(fn) { usageEditorLoad = fn; }},
		'save': {'give': saveUsageCode, 'take': function(fn) { usageEditorSave = fn; }},
		'focus': {'take': function(fn) { usageEditorFocus = fn; }},
		'onExpand': expandToUsageView,
		'onRetract': setInCodeEditionMode});
	
	makeChunkEditor(document.getElementById('usageChunkEditor'),
		{'load': {'give': loadUsageCode, 'take': function(fn) { usageChunkEditorLoad = fn; }},
		'save': {'give': saveUsageCode, 'take': function(fn) { usageChunkEditorSave = fn; }},
		'focus': {'take': function(fn) { usageChunkEditorFocus = fn; }},
		'onRetract': codeChunkEditorFocus});
	setInCodeEditionMode();
	
	/** Render message box **/
	$('#menu').append('<div id="messageBox"></div>');
	displayMessages();
	
	/** Render the command line **/
	$('#menu').append('<div id="commandLinePlace"></div>');
	React.renderComponent(
	  <CommandLine options={{'focus': {'take': function(fn) { commandFocus = fn; }},
							'select': {'give': function(moduleName, systemName, ids) {
								guardedSave(function() {
									deactivateTopMaintenanceTask();
									setMaintenanceMode(false);
									setInCodeEditionMode(moduleName, systemName, ids);
								});
							}}}} />,
	  document.getElementById('commandLinePlace')
	);
	
	/** Render the task list **/
	$('#menu').append('<div id="tasks"></div>');
	React.renderComponent(
		<TaskBar options={{'activate': {'take': function(fn) { activateTopMaintenanceTask = fn; }},
						'deactivate': {'take': function(fn) { deactivateTopMaintenanceTask = fn; }},
						'onTaskChange': setTaskEditionMode}} />,
		document.getElementById('tasks')
	);
	
	/** Global keybindings **/
	var keyMap = {'basic': {}}, map = keyMap.basic;
	map['Ctrl-1'] = function(e) {
		e.preventDefault();
		e.stopPropagation();
		commandFocus();
	};
	map['Ctrl-Q'] = function(e) {
		var saveResult;
		
		e.preventDefault();
		e.stopPropagation();
		
		saveResult = save();
		
		if(!saveResult.done && saveResult.ok) {
			activateTopMaintenanceTask(setTaskEditionMode);
		} else if(saveResult.ok) {
			clearMessages();
		}
	};
	globalKeyBinder = makeGlobalKeyBinder({}, keyMap, 'basic');
	
	/** Save on closing the tab or changing its address **/
	window.addEventListener('beforeunload', function(event) {
		var message = "Some work still need to be done. Close anyway? (your work will be lost)";
		if(!save().ok) {
			(event || window.event).returnValue = message;
			return message;
		}
	});
	
	/** At last, load code based on URL params, if any */
	querySearch = /[\?&]moduleName=([^&]*)/.exec(window.location.search);
	if(querySearch) {
		moduleName = querySearch[1];
	}
	
	querySearch = /[\?&]systemName=([^&]*)/.exec(window.location.search);
	if(querySearch) {
		systemName = querySearch[1];
	}
	
	if(moduleName && systemName) {
		loadCode(moduleName, systemName);
	}
});