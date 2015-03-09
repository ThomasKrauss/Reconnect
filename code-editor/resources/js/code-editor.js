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
	// adapting communications
	var loadCode, saveCode, loadUsageCode, saveUsageCode;
	
	// about messages
	var displayedMessages;
	
	// the component verbs
	var commandFocus;
	var activateTopMaintenanceTask, deactivateTopMaintenanceTask;
	var codeStreamEditorLoad, codeStreamEditorSave, codeStreamEditorFocus;
	var codeChunkEditorLoad, codeChunkEditorSave, codeChunkEditorFilter, codeChunkEditorFocus;
	var usageStreamEditorLoad, usageStreamEditorSave, usageStreamEditorFocus;
	var usageChunkEditorLoad, usageChunkEditorSave, usageChunkEditorFocus;
	
	var displayMessages = function(messages) {
		displayedMessages = messages;
		React.renderComponent(
		  <MessageBox messages={displayedMessages} />,
		  document.getElementById('messageBox')
		);
	};
	
	var addMessages = function(messages) {
		displayMessages(displayedMessages.concat(messages));
	};
	
	loadCode = function(systemName, moduleName) {
		var result = loadChunks(moduleName, systemName);
		addMessages(result.messages);
		return result.chunks;
	};
	
	saveCode = function(additions, replacements, deletions, orderedIds) {
		return saveChunks(JSON.stringify(additions), JSON.stringify(replacements), JSON.stringify(deletions), JSON.stringify(orderedIds));
	};
	
	loadUsageCode = function(systemName, moduleName, actionName) {
		var result = loadUsageChunks(actionName, moduleName, systemName);
		addMessages(result.messages);
		return result.chunks;
	};
	
	saveUsageCode = function(additions, replacements, deletions, orderedIds) {
		return saveUsageChunks(JSON.stringify(additions), JSON.stringify(replacements), JSON.stringify(deletions), JSON.stringify(orderedIds));
	};

	var getResourceMessages = function(endPointName, entry, callback) {
		var messages = [], exclude = {'usages':['ok']};
		var isDataExcluded = function(endPointName, dataKey) {
			return isArray(exclude[endPointName]) && exclude[endPointName].indexOf(dataKey) !== -1;
		};
		
		var depthLimit = entry.length === 2 ? 1 : 0;
		var ignoreLimit = entry.length === 2 ? 0 : -1;
		
		EndPoints.getResources([endPointName].concat(entry).join('/'), depthLimit, ignoreLimit, function(data) {
			if(endPointName === 'usages') {
			}
			myMap(data, function(item) {
				var inProblem = false, entry;
				
				myMap(item.value && item.value.stats, function(key, value) {
					inProblem = inProblem || (!isDataExcluded(endPointName, key) && value > 0);
				});
				
				if(inProblem) {
					messages = messages.concat(item.value.messages);
				}
			});
			
			callback(messages);
		});
	};
	
	var makeEditionManager = function() {
		var editionManager;
		var selectedModuleName, selectedSystemName; // what is the edited module of what system?
		var codeSaveMessages, usageSaveMessages; // TODO
		var focusedActionName, inMaintenanceMode; // are we editing tests of a given action? are we in maintenance mode?
		var savedSystemName, savedModuleName, savedFocusedActionName, showMaintenanceDone;
		
		/** INTERNAL HELPERS **/
		
		// Generate messages about work-in-progress chunks and chunk the save operation has missed
		var generateSaveMessages = function(wipChunkCount, missedChunkCount) {
			var messages = [];
			
			if(wipChunkCount > 0) {
				messages = messages.concat([{'id': 'Work in progress', 'error': [wipChunkCount+' chunk'+(wipChunkCount === 1 ? ' is' : 's are')+' unfinished. Save aborted.']}]);
			}
			
			if(missedChunkCount > 0) {
				messages = messages.concat([{'id': 'Chunk missed', 'warning': [missedChunkCount+' chunk'+(missedChunkCount === 1 ? ' was' : 's were')+' missed during saving.']}]);
			}
			
			return messages;
		};
		
		var generateMaintenanceDoneMessage = function() {
			return [{'id': 'Maintenance', 'ok': ['Done.']}];
		};
		
		// saveResult = {'ok': <boolean>, 'someWorkDone': <boolean>, 'messages': Array}
		// when ok is true, it means all went well (no WIP chunks, no chunk missed and no other problems)
		// when someWorkDone is true, it means the save operation performed something, saving some chunks or deleting some chunks or both
		var saveTemplate = function() {
			var someWorkDone = false, ok = true;
			var messages;
			
			messages = myMap(arguments, function(index, saveFunction) {
				var data = saveFunction(), messages;
				
				if((isArray(data.chunks) && data.chunks.length > 0) || data.deleteCount > 0 || data.reordered) {
					someWorkDone = true;
				}
				
				messages = generateSaveMessages(data.wipChunkCount, data.missedChunkCount).concat(data.messages);
				
				if(messages.length > 0) {
					ok = false;
				}
				
				return messages;
			});
			
			return {'ok': ok, 'someWorkDone': someWorkDone, 'messages': messages};
		};
		
		// These 6 wrappers over the editors' save functions capture the messages of each editor to allow a correct refresh of messages whatever the state is (maintenance or not, editing usages or not)
		var saveCodeStreamEditorContent = function() {
			var saveResult = saveTemplate(codeStreamEditorSave);
			codeSaveMessages = saveResult.messages[0];
			return saveResult;
		};
		
		var saveUsageStreamEditorContent = function() {
			var saveResult = saveTemplate(usageStreamEditorSave);
			usageSaveMessages = saveResult.messages[0];
			return saveResult;
		};
		
		var saveStreamEditors = function() {
			var saveResult = saveTemplate(codeStreamEditorSave, usageStreamEditorSave);
			codeSaveMessages = saveResult.messages[0];
			usageSaveMessages = saveResult.messages[1];
			return saveResult;
		};
		
		var saveCodeChunkEditorContent = function() {
			var saveResult = saveTemplate(codeChunkEditorSave);
			codeSaveMessages = saveResult.messages[0];
			return saveResult;
		};
		
		var saveUsageChunkEditorContent = function() {
			var saveResult = saveTemplate(usageChunkEditorSave);
			usageSaveMessages = saveResult.messages[0];
			return saveResult;
		};
		
		var saveChunkEditors = function() {
			var saveResult = saveTemplate(codeChunkEditorSave, usageChunkEditorSave);
			codeSaveMessages = saveResult.messages[0];
			usageSaveMessages = saveResult.messages[1];
			return saveResult;
		};
		
		// display the save messages about the code editor with the problems of compilation
		var displayUpToCompileProblemsMessages = function(initialMessages) {
			var messages = isArray(initialMessages) ? initialMessages : [];

			if(showMaintenanceDone) {
				showMaintenanceDone = false;
				messages = messages.concat(generateMaintenanceDoneMessage());
			}
			messages = messages.concat(codeSaveMessages);
			
			getResourceMessages('compile-problems', [selectedSystemName, selectedModuleName],
					function(compileProblemsMessages) {
						displayMessages(messages.concat(compileProblemsMessages));
					});
		};
		
		// display all the save messages with compile problems and usage messages
		var displayUpToUsagesMessages = function(initialMessages) {
			var messages = isArray(initialMessages) ? initialMessages : [];
			
			if(showMaintenanceDone) {
				showMaintenanceDone = false;
				messages = messages.concat(generateMaintenanceDoneMessage());
			}
			messages = messages.concat(codeSaveMessages, usageSaveMessages);
			
			getResourceMessages('compile-problems', [selectedSystemName, selectedModuleName],
					function(compileProblemsMessages) {
						getResourceMessages('usages', [selectedSystemName, selectedModuleName, focusedActionName], function(usagesMessages) {
							displayMessages(messages.concat(compileProblemsMessages, usagesMessages));
						});
					});
		};
		
		// display the appropriate save messages and either compile problems messages or usages messages
		var displayMaintenanceMessages = function(callback) {
			var saveMessages = codeSaveMessages;
			
			if(focusedActionName) {
				saveMessages = saveMessages.concat(usageSaveMessages);
			}
			
			getResourceMessages(inMaintenanceMode, [selectedSystemName, selectedModuleName],
				function(messages) {
					displayMessages(saveMessages.concat(messages));
					if(isFunction(callback)) {
						callback(messages);
					}
				});
		};
		
		// whether we are in maintenance or not, we displayUpToCompileProblemsMessages when only the code editor is up or we displayUpToUsagesMessages when both editors are up
		var onSaveError = function() {
			if(focusedActionName) {
				makeDebuggedUsagesFile(focusedActionName, selectedModuleName, selectedSystemName);
				displayUpToUsagesMessages();
			} else {
				displayUpToCompileProblemsMessages();
			}
		};
		
		// guard for saving, call onSaveError if the save was not ok, call the given onSuccess otherwise
		var guard = function(saveFunction, onSuccess, errorIfSomeWorkDone) {
			var saveResult = saveFunction();
			var doGuard = errorIfSomeWorkDone && saveResult.someWorkDone;
			
			if(saveResult.ok && !doGuard && isFunction(onSuccess)) {
				onSuccess();
			} else {
				onSaveError();
			}
			
			return saveResult;
		};
		
		// get the save function needed to be called before switching to another situation
		var getSaveFunction = function() {
			var saveFunction;
			
			if(inMaintenanceMode) {
				if(focusedActionName) {
					saveFunction = saveChunkEditors;
				} else {
					saveFunction = saveCodeChunkEditorContent;
				}
			} else {
				if(focusedActionName) {
					saveFunction = saveStreamEditors;
				} else {
					saveFunction = saveCodeStreamEditorContent;
				}
			}
			
			return saveFunction;
		};
		
		/** EXTERNALS **/
		
		var switchToNormalMode = function(systemName, moduleName) {
			guard(getSaveFunction(), function() {
				selectedSystemName = systemName;
				selectedModuleName = moduleName;
				focusedActionName = null;
				inMaintenanceMode = null;
				deactivateTopMaintenanceTask();
				deleteDebuggedUsagesFile();
				
				$('#codeStreamEditor').show().css({'visibility': 'hidden'});
				$('#codeChunkEditor').hide();
				$('#usageStreamEditor').hide();
				$('#usageChunkEditor').hide();
				$('#layout').css({'width': '750px'});
				
				displayUpToCompileProblemsMessages(codeStreamEditorLoad(systemName, moduleName));
				
				$('#codeStreamEditor').css({'visibility': 'visible'});
				codeStreamEditorFocus();
			});
		};
		
		var switchToNormalModeWithUsageEditor = function(systemName, moduleName, actionName) {
			guard(getSaveFunction(), function() {
				selectedSystemName = systemName;
				selectedModuleName = moduleName;
				focusedActionName = actionName;
				inMaintenanceMode = null;
				deactivateTopMaintenanceTask();
				makeDebuggedUsagesFile(focusedActionName, selectedModuleName, selectedSystemName);
				
				$('#codeStreamEditor').show().css({'visibility': 'hidden'});
				$('#codeChunkEditor').hide();
				$('#usageStreamEditor').show().css({'visibility': 'hidden'});
				$('#usageChunkEditor').hide();
				$('#layout').css({'width': '1510px'});
				
				displayUpToUsagesMessages(codeStreamEditorLoad(systemName, moduleName));
				usageStreamEditorLoad(selectedSystemName, selectedModuleName, focusedActionName);
				
				$('#codeStreamEditor').css({'visibility': 'visible'});
				$('#usageStreamEditor').css({'visibility': 'visible'});
				usageStreamEditorFocus();
			});
		};
		
		var switchToMaintenanceMode = function(preventSwitchingIfSomeWorkDone) {
			var errorIfSomeWorkDone = !inMaintenanceMode && preventSwitchingIfSomeWorkDone;
			
			guard(getSaveFunction(), function() {
				var item = activateTopMaintenanceTask();
				
				if(item) {
					deleteDebuggedUsagesFile();
				
					$('#codeStreamEditor').hide();
					$('#codeChunkEditor').show().css({'visibility': 'hidden'});
					$('#usageStreamEditor').hide();
					$('#usageChunkEditor').hide();
					$('#layout').css({'width': '750px'});
					
					if(inMaintenanceMode === null) {
						savedSystemName = selectedSystemName;
						savedModuleName = selectedModuleName;
						savedFocusedActionName = focusedActionName;
					}
					
					inMaintenanceMode = item.endPointName;
					selectedSystemName = item.entry[0];
					selectedModuleName = item.entry[1];
					focusedActionName = null;
					
					displayMaintenanceMessages(function(messages) {
						codeChunkEditorLoad(selectedSystemName, selectedModuleName);
						codeChunkEditorFilter(myMap(messages, function(message) {
							return message.id;
						}));
						$('#codeChunkEditor').css({'visibility': 'visible'});
						codeChunkEditorFocus();
					});
				} else {
					if(savedSystemName && savedModuleName) {
						showMaintenanceDone = true;
						if(savedFocusedActionName) {
							switchToNormalModeWithUsageEditor(savedSystemName, savedModuleName, savedFocusedActionName);
						} else {
							switchToNormalMode(savedSystemName, savedModuleName);
						}
						savedSystemName = savedModuleName = savedFocusedActionName = null;
					} else {
						selectedSystemName = null;
						selectedModuleName = null;
						focusedActionName = null;
						deactivateTopMaintenanceTask();
						deleteDebuggedUsagesFile();
						
						if(inMaintenanceMode !== null) {
							inMaintenanceMode = null;
							$('#codeStreamEditor').hide();
							$('#codeChunkEditor').hide();
							$('#usageStreamEditor').hide();
							$('#usageChunkEditor').hide();
							
							displayMessages(generateMaintenanceDoneMessage());
						}
					}
				}
			}, errorIfSomeWorkDone);
		};
		
		var displayOnlyCodeStreamEditor = function() {
			guard(saveUsageStreamEditorContent, function() {
				focusedActionName = null;
				deleteDebuggedUsagesFile();
				
				$('#usageStreamEditor').hide();
				$('#layout').css({'width': '750px'});
				codeStreamEditorFocus();
				
				displayUpToCompileProblemsMessages();
			});
		};
		
		var displayOnlyCodeChunkEditor = function() {
			guard(saveUsageChunkEditorContent, function() {
				focusedActionName = null;
				deleteDebuggedUsagesFile();
				
				$('#usageChunkEditor').hide();
				$('#layout').css({'width': '750px'});
				codeChunkEditorFocus();
				
				displayMaintenanceMessages();
			});
		};
		
		// when we display both editors, whether in maintenance or not, we need to guard saving the usage editor's content because we may load other usages while the usage editor was in view, meaning while some different usages were edited
		var displayBothStreamEditors = function(actionName) {
			guard(saveUsageStreamEditorContent, function() {
				focusedActionName = actionName;
				makeDebuggedUsagesFile(focusedActionName, selectedModuleName, selectedSystemName);
				
				$('#usageStreamEditor').show().css({'visibility': 'hidden'});
				$('#layout').css({'width': '1510px'});
				
				displayUpToUsagesMessages();
				usageStreamEditorLoad(selectedSystemName, selectedModuleName, focusedActionName);
				
				$('#usageStreamEditor').css({'visibility': 'visible'});
				usageStreamEditorFocus();
			});
		};
		
		var displayBothChunkEditors = function(actionName) {
			guard(saveUsageChunkEditorContent, function() {
				focusedActionName = actionName;
				makeDebuggedUsagesFile(focusedActionName, selectedModuleName, selectedSystemName);
				
				$('#usageChunkEditor').show().css({'visibility': 'hidden'});
				$('#layout').css({'width': '1510px'});
				
				displayMaintenanceMessages();
				usageChunkEditorLoad(selectedSystemName, selectedModuleName, focusedActionName);
				
				$('#usageChunkEditor').css({'visibility': 'visible'});
				usageChunkEditorFocus();
			});
		};
		
		var save = function() {
			return guard(getSaveFunction());
		};
		
		var reset = function() {
			editionManager = {};
			
			selectedSystemName = null;
			selectedModuleName = null;
						
			codeSaveMessages = [];
			usageSaveMessages = [];
			
			focusedActionName = null;
			inMaintenanceMode = null;
			
			savedSystemName = null;
			savedModuleName = null;
			showMaintenanceDone = false;
			
			editionManager.switchToNormalMode = switchToNormalMode;
			editionManager.switchToMaintenanceMode = switchToMaintenanceMode;
			editionManager.displayOnlyCodeStreamEditor = displayOnlyCodeStreamEditor;
			editionManager.displayBothStreamEditors = displayBothStreamEditors;
			editionManager.displayOnlyCodeChunkEditor = displayOnlyCodeChunkEditor;
			editionManager.displayBothChunkEditors = displayBothChunkEditors;
			editionManager.save = save;
		};
		reset();
		return editionManager;
	};
	
	editionManager = makeEditionManager();
	deleteDebuggedUsagesFile();
	
	$('#layout').append('<div id="codeStreamEditor"></div>');
	$('#layout').append('<div id="usageStreamEditor"></div>');
	$('#layout').append('<div id="codeChunkEditor"></div>');
	$('#layout').append('<div id="usageChunkEditor"></div>');
	
	/** Render editors **/
	makeChunkStreamEditor(document.getElementById('codeStreamEditor'),
		{'load': {'give': loadCode, 'take': function(fn) { codeStreamEditorLoad = fn; }},
		'save': {'give': saveCode, 'take': function(fn) { codeStreamEditorSave = fn; }},
		'focus': {'take': function(fn) { codeStreamEditorFocus = fn; }},
		'onExpand': function(chunk) {
			if(isActionDefinition(chunk)) {
				editionManager.displayBothStreamEditors(chunk.id);
			}
		},
		'onRetract': editionManager.displayOnlyCodeStreamEditor});
	
	makeChunkEditor(document.getElementById('codeChunkEditor'),
		{'load': {'give': loadCode, 'take': function(fn) { codeChunkEditorLoad = fn; }},
		'save': {'give': saveCode, 'take': function(fn) { codeChunkEditorSave = fn; }},
		'filter': {'take': function(fn) { codeChunkEditorFilter = fn; }},
		'focus': {'take': function(fn) { codeChunkEditorFocus = fn; }},
		'onExpand': function(chunk) {
			if(isActionDefinition(chunk)) {
				console.log('displayBothChunkEditors '+chunk.id)
				editionManager.displayBothChunkEditors(chunk.id);
			}
		}});
	
	makeChunkStreamEditor(document.getElementById('usageStreamEditor'),
		{'load': {'give': loadUsageCode, 'take': function(fn) { usageStreamEditorLoad = fn; }},
		'save': {'give': saveUsageCode, 'take': function(fn) { usageStreamEditorSave = fn; }},
		'focus': {'take': function(fn) { usageStreamEditorFocus = fn; }},
		'onRetract': editionManager.displayOnlyCodeStreamEditor});
	
	makeChunkEditor(document.getElementById('usageChunkEditor'),
		{'load': {'give': loadUsageCode, 'take': function(fn) { usageChunkEditorLoad = fn; }},
		'save': {'give': saveUsageCode, 'take': function(fn) { usageChunkEditorSave = fn; }},
		'focus': {'take': function(fn) { usageChunkEditorFocus = fn; }},
		'onRetract': editionManager.displayOnlyCodeChunkEditor});
	
	/** Render message box **/
	$('#menu').append('<div id="messageBox"></div>');
	
	/** Render the command line **/
	$('#menu').append('<div id="commandLinePlace"></div>');
	React.renderComponent(
	  <CommandLine options={{'focus': {'take': function(fn) { commandFocus = fn; }},
							'select': {'give': function(moduleName, systemName) {
								editionManager.switchToNormalMode(systemName, moduleName);
							}}}} />,
	  document.getElementById('commandLinePlace')
	);
	
	/** Render the task list **/
	$('#menu').append('<div id="tasks"></div>');
	React.renderComponent(
		<TaskBar options={{'activate': {'take': function(fn) { activateTopMaintenanceTask = fn; }},
							'deactivate': {'take': function(fn) { deactivateTopMaintenanceTask = fn; }},
							'onTaskChange': editionManager.switchToMaintenanceMode}} />,
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
		e.preventDefault();
		e.stopPropagation();
		editionManager.switchToMaintenanceMode(true);
	};
	globalKeyBinder = makeGlobalKeyBinder({}, keyMap, 'basic');
	
	/** Save on closing the tab or changing its address **/
	window.addEventListener('beforeunload', function(event) {
		var message = "Some work still need to be done. Close anyway? (your work will be lost)";
		if(!editionManager.save().ok) {
			(event || window.event).returnValue = message;
			return message;
		}
	});
	
	/** At last, load code based on URL params, if any */
	var loadOnStart = function() {
		var moduleName, systemName, querySearch;
		
		querySearch = /[\?&]moduleName=([^&]*)/.exec(window.location.search)
		if(querySearch) {
			moduleName = querySearch[1];
		}
		
		querySearch = /[\?&]systemName=([^&]*)/.exec(window.location.search);
		if(querySearch) {
			systemName = querySearch[1];
		}
		
		if(moduleName && systemName) {
			editionManager.switchToNormalMode(systemName, moduleName);
		}
	};
	loadOnStart();
});