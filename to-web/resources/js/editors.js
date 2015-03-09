/** @jsx React.DOM */
function makeCodeSpace() {
	var codeSpace, allChunks;
	
	/**
	* Select the chunks that needs to be focused, all if no focus is set.
	*/
	var selectActionChunks = function(actionNames) {
		var selectedChunks = null, count, i, chunk;
		
		if(actionNames && actionNames.length > 0) {
			selectedChunks = [];
			count = 0;
			i = 0;
			while(i < allChunks.length && count < actionNames.length) {
				chunk = allChunks[i];
				if(isActionDefinition(chunk) && actionNames.indexOf(chunk.id) !== -1) {
					selectedChunks.push(chunk);
					count++;
				}
				i++;
			}
		}
		
		return selectedChunks ? selectedChunks : allChunks;
	};
	
	/**
	* Reset the space display: new chunks and no more priorizing.
	*/
	var resetChunks = function(chunks) {
		allChunks = chunks;
		codeSpace.spaceOfInterestChanged(allChunks);
	};
	
	var addChunks = function(chunks) {
		var i, oldPos, pos, chunk, targetChunks;
		
		if(isArray(chunks)) {
			targetChunks = clone(allChunks);
			oldPos = -1;
			for(i=0; i<chunks.length; i++) {
				chunk = chunks[i];
				
				// Add or replace to known chunks
				pos = positionOfChunk(chunk, targetChunks);
				if(pos === -1) {
					if(oldPos === -1) {
						targetChunks.unshift(chunk);
						oldPos = 0;
					} else {
						oldPos++;
						targetChunks.splice(oldPos,0,chunk);
					}
				} else {
					targetChunks.splice(pos,1,chunk);
					oldPos = pos;
				}
			}
			
			allChunks = targetChunks;
		}
		
		return codeSpace;
	};
	
	var filterActions = function(actionNames) {
		var selectedChunks;
		if(isArray(actionNames) && actionNames.length > 0) {
			selectedChunks = selectActionChunks(actionNames);
		} else {
			selectedChunks = allChunks;
		}
		codeSpace.spaceOfInterestChanged(selectedChunks);
	};
	
	var reset = function() {
		codeSpace = {};
		allChunks = [];
		
		GlobalEvents.registerAsEmitter(codeSpace, 'spaceOfInterestChanged');
		codeSpace.resetChunks = resetChunks;
		codeSpace.addChunks = addChunks;
		codeSpace.filterActions = filterActions;
	};
	reset();
	return codeSpace;
}

// BIG ASSUMPTION: only one chunk per editor ever!!

var makeChunkSaveWrapper = function(saveFn) {
	var wrapper;
	
	var countWipChunk = function(chunks) {
		var count = 0;
		myMap(chunks, function(chunk) {
			if(chunk.wip) {
				count++;
			}
		});
		return count;
	};
	
	var save = function(editorList) {
		var result;
		var chunks = [], editorReferenceChunks = [], editorIndexes = [], editors = [];
		var wipChunkCount = 0, missedChunkCount = 0;
		
		myMap(editorList, function(editor) {
			var editorChunks, count;
			
			if(!editor.isClean()) {
				editors.push(editor);
				
				editorChunks = editor.getChunks();
				editorReferenceChunks.push(editorChunks[0]);
				count = countWipChunk(editorChunks);
				chunks = chunks.concat(editorChunks);
				
				if(count > 0) {
					wipChunkCount += count;
					editor.emphasize();
				} else {
					editor.indentAll();
				}
			}
		});
		
		if(chunks.length > 0 && wipChunkCount === 0) {
			result = saveFn(null, chunks, null, null);
			
			myMap(result && result.chunks, function(chunk) {
				var index = -1, referenceChunk, i = 0;
				while(index === -1 && i<editorReferenceChunks.length) {
					referenceChunk = editorReferenceChunks[i];
					if(chunk.id === referenceChunk.id
						&& chunk.type === referenceChunk.type
						&& chunk.actionName === referenceChunk.actionName
						&& chunk.moduleName === referenceChunk.moduleName
						&& chunk.systemName === referenceChunk.systemName) {
						index = i;
					}
					i++;
				}
				if(index !== -1) {
					editors[index].contentIsSaved();
				} else {
					missedChunkCount++;
				}
			});
		} else {
			result = {};
		}
		
		if(!result.messages) {
			result.messages = [];
		}
		result.wipChunkCount = wipChunkCount;
		result.missedChunkCount = missedChunkCount;
		result.deleteCount = 0;
		result.reordered = false;
		
		return result;
	};
	
	var reset = function() {
		wrapper = {};
		
		wrapper.save = save;
	};
	reset();
	return wrapper;
};

var makeChunkStreamSaveWrapper = function(saveFn) {
	var wrapper;
	var editedChunks;
	
	var scanForChunksToSave = function(chunks) {
		var toAdd = [], toReplace = [], scannedIndexes = [], wipChunkCount = 0, reordered = false;
		
		myMap(chunks, function(chunk, index) {
			var referenceIndex;
			
			referenceIndex = positionOfChunk(chunk, editedChunks);
			if(referenceIndex === -1) {
				if(chunk.wip) {
					wipChunkCount++;
				} else {
					toAdd.push(chunk);
				}
			} else {
				if(referenceIndex !== index) {
					reordered = true;
				}
				scannedIndexes.push(referenceIndex);
				if(chunk.wip) {
					wipChunkCount++;
				} else if(chunk.code !== editedChunks[referenceIndex].code) {
					toReplace.push(chunk);
				}
			}
		});
		
		return {'toAdd': toAdd, 'toReplace': toReplace, 'scannedIndexes': scannedIndexes, 'wipChunkCount': wipChunkCount, 'reordered': reordered};
	};
	
	var getChunksToDelete = function(scannedIndexes) {
		var toDelete = [], index;
		
		if(scannedIndexes.length !== editedChunks.length) {
			scannedIndexes.sort(function(a, b) { return a > b; });
			index = 0;
			
			myMap(scannedIndexes, function(value) {
				while(index < value) {
					toDelete.push(editedChunks[index]);
					index++;
				}
				index++;
			});
			
			while(index < editedChunks.length) {
				toDelete.push(editedChunks[index]);
				index++;
			}
		}
		
		return toDelete;
	};
	
	var save = function(editor) {
		var result = {};
		var missedChunkCount = 0, chunks, toDelete = [], scanResult, shouldSave;
		
		if(editor && !editor.isClean()) {
			chunks = editor.getChunks();
			
			scanResult = scanForChunksToSave(chunks);
			toDelete = getChunksToDelete(scanResult.scannedIndexes);
			editedChunks = chunks;
			
			shouldSave = scanResult.toAdd.length > 0 || scanResult.toReplace.length > 0 || toDelete.length > 0 || scanResult.reordered;
			
			if(scanResult.wipChunkCount === 0) {
				if(shouldSave) {
					result = saveFn(scanResult.toAdd, scanResult.toReplace, toDelete, myMap(chunks, makeChunkKey));
					
					myMap(result && result.chunks, function(chunk) {
						var index = -1, referenceChunk, i = 0;
						while(index === -1 && i<editedChunks.length) {
							referenceChunk = editedChunks[i];
							if(chunk.id === referenceChunk.id
								&& chunk.type === referenceChunk.type
								&& chunk.actionName === referenceChunk.actionName
								&& chunk.moduleName === referenceChunk.moduleName
								&& chunk.systemName === referenceChunk.systemName) {
								index = i;
							}
							i++;
						}
						if(index === -1) {
							missedChunkCount++;
						}
					});
					
					if(missedChunkCount === 0) {
						editor.contentIsSaved();
					} else {
						editor.emphasize();
					}
				}
			} else {
				editor.emphasize();
			}
		}
		
		if(!result.messages) {
			result.messages = [];
		}
		result.wipChunkCount = scanResult ? scanResult.wipChunkCount : 0;
		result.missedChunkCount = missedChunkCount;
		result.deleteCount = toDelete.length;
		result.reordered = scanResult ? scanResult.reordered : false;
		
		return result;
	};
	
	var setEditedChunks = function(chunks) {
		editedChunks = chunks;
	};
	
	var reset = function() {
		wrapper = {};
		editedChunks = [];
		
		wrapper.save = save;
		wrapper.setEditedChunks = setEditedChunks;
	};
	reset();
	return wrapper;
};

var makeDefinitionEditor = function(domElement, initChunk, options) {
	var definitionEditor, codeMirrorEditor;
	var generation, chunk;
		
	var refreshGeneration = function() {
		generation = codeMirrorEditor.changeGeneration();
		return definitionEditor;
	};
	
	var indentAll = function() {
		var lineCount = codeMirrorEditor.lineCount();
		codeMirrorEditor.operation(function() {
			var i;
			for(i = 0; i<lineCount; i++) {
				codeMirrorEditor.indentLine(i);
			}
		});
		return definitionEditor;
	};
	
	var getEditedSystemName = function() {
		return chunk.systemName;
	};
	
	var getChunks = function() {
		var code;
		
		// always export properly indented code, especially since the id of fact chunks is the code itself
		indentAll();
		
		if(arguments && arguments['1']) {
			code = codeMirrorEditor.getRange(arguments['0'], arguments['1']);
		} else {
			code = codeMirrorEditor.getValue();
		}
		
		return identifyChunks(code, chunk.systemName, chunk.moduleName, chunk.actionName);
	};
	
	var contentIsSaved = function() {
		refreshGeneration();
		deemphasize();
		return blink();
	};
	
	var isClean = function() {
		return codeMirrorEditor.isClean(generation);
	};
	
	var focus = function() {
		codeMirrorEditor.focus();
		return definitionEditor;
	};
	
	var refresh = function() {
		codeMirrorEditor.refresh();
		return definitionEditor;
	};
	
	var emphasize = function() {
		$(domElement).addClass('emphasized');
		return definitionEditor;
	};
	
	var deemphasize = function() {
		$(domElement).removeClass('emphasized');
		return definitionEditor;
	};
	
	var blink = function() {
		$(codeMirrorEditor.getWrapperElement()).animate({
			borderColor: "#d33682"
		},400).animate({
			borderColor: (codeMirrorEditor.hasFocus() ? "#268bd2" : "#002b30")
		},250, function() {
			$(this).css('borderColor','');
		});
		return definitionEditor;
	};
	
	var setChunk = function(newChunk) {
		chunk = newChunk;
		codeMirrorEditor.setValue(newChunk.code);
		refreshGeneration().indentAll().deemphasize();
	};
	
	var reset = function() {
		definitionEditor = {};
		$(domElement).empty();
		chunk = initChunk;
		
		codeMirrorEditor =  CodeMirror(domElement,
			{mode: "commonlisp",
			value: initChunk.code,
			tabSize: 2,
			autofocus: false,
			dragDrop: false,
			lineNumbers: false,
			lineWrapping : true,
			matchBrackets: true,
			viewportMargin: Infinity,
			keyMap: 'reconnect'});
		
		if(isFunction(options.onFocus)) {
			codeMirrorEditor.on('focus', function() {
				options.onFocus(getChunks());
			});
		}
		
		indentAll();
		refreshGeneration();
		
		codeMirrorEditor.getEditedSystemName = getEditedSystemName;
		codeMirrorEditor.getChunks = getChunks;
		codeMirrorEditor.expand = function(chunk) {
			if(isFunction(options.onExpand)) {
				options.onExpand(chunk);
			}
		};
		codeMirrorEditor.retract = function(chunk) {
			if(isFunction(options.onRetract)) {
				options.onRetract(chunk);
			}
		};
		if(isFunction(options.ctrlUp)) {
			codeMirrorEditor.ctrlUp = options.ctrlUp;
		}
		if(isFunction(options.ctrlDown)) {
			codeMirrorEditor.ctrlDown = options.ctrlDown;
		}
		
		definitionEditor.indentAll = indentAll;
		definitionEditor.getEditedSystemName = getEditedSystemName;
		definitionEditor.getChunks = getChunks;
		definitionEditor.isClean = isClean;
		definitionEditor.contentIsSaved = contentIsSaved;
		definitionEditor.focus = focus;
		definitionEditor.refresh = refresh;
		definitionEditor.emphasize = emphasize;
		definitionEditor.deemphasize = deemphasize;
		definitionEditor.blink = blink;
		definitionEditor.setChunk = setChunk;
	};
	
	reset();
	return definitionEditor;
};

var makeChunkEditor = function(domElement, options) {
	var chunkEditor;
	var editorList, saveWrapper, codeSpace, focusedEditorIndex;
	
	var load = function(systemName, moduleName, actionName) {
		codeSpace.resetChunks(options.load.give.apply(null, [systemName, moduleName, actionName]));
	};
			
	var filter = function(actionNames) {
		if(options.filter && isFunction(options.filter.take)) {
			codeSpace.filterActions(actionNames);
		} else {
			// if the filter has not been taken, it means filtering is of no interest
			// it is used only after loading and everything should just be displayed
			// current uses: filter code chunks and no filter of usage chunks
			codeSpace.filterActions();
		}
	};
	
	var focus = function() {
		if(editorList.length > 0) {
			editorList[0].focus();
		}
	};
	
	var save = function() {
		var saveResult = saveWrapper.save(editorList);
		codeSpace.addChunks(saveResult.chunks);
		return saveResult;
	};
	
	var refresh = function(chunks) {
		var listRoot, html = '<ul class="editor">';
		html += myMap(chunks, function() { return '<li></li>'; }).join('');
		html += '</ul>';
		
		$(domElement).html(html);
		listRoot = ($(domElement).children())[0];
		focusedEditorIndex = -1;
		
		editorList = myMap($(listRoot).children().toArray(), function(domElement, index) {
			return makeDefinitionEditor(domElement, chunks[index],
				{'onFocus': function() {
					focusedEditorIndex = index;
				},
				'ctrlUp': function() {
					if(focusedEditorIndex > 0) {
						editorList[focusedEditorIndex - 1].focus();
					}
				},
				'ctrlDown': function() {
					if(focusedEditorIndex > -1 && focusedEditorIndex + 1 < editorList.length) {
						editorList[focusedEditorIndex + 1].focus();
					}
				},
				'onExpand': options.onExpand,
				'onRetract': options.onRetract});
		});
		
		focus();
	};
	
	var reset = function() {
		chunkEditor = {};
		
		chunks = [];
		editorList = [];
		saveWrapper = makeChunkSaveWrapper(options.save.give);
		codeSpace = makeCodeSpace();
		
		options.load.take(load);
		options.save.take(save);
		if(options.filter && isFunction(options.filter.take)) {
			options.filter.take(filter);
		}
		if(options.focus && isFunction(options.focus.take)) {
			options.focus.take(focus);
		}
		
		codeSpace.onSpaceOfInterestChanged(refresh);
	};
	reset();
	return chunkEditor;
};

var makeChunkStreamEditor = function(domElement, options) {
	var chunkStreamEditor;
	var editor, saveWrapper;
	var editorUI, titleUI;
	
	var load = function(systemName, moduleName, actionName) {
		var loadResult = options.load.give.apply(null, [systemName, moduleName, actionName]);
		
		$(titleUI).html('<p>'+(actionName ? actionName+" " : "")+moduleName+" "+systemName+'</p>');
		editor.setChunk(makeChunkStream(loadResult.chunks, systemName, moduleName, actionName));
		saveWrapper.setEditedChunks(editor.getChunks());
		
		return loadResult.messages;
	};
	
	var focus = function() {
		if(editor) {
			editor.focus().refresh();
		}
	};
	
	var save = function() {
		return saveWrapper.save(editor);
	};
	
	var reset = function() {
		chunkStreamEditor = {};
		saveWrapper = makeChunkStreamSaveWrapper(options.save.give);
		
		$(domElement).html('<div class="title"></div><div class="editor"></div>');
		editorUI = $(domElement).children('.editor')[0];
		titleUI = $(domElement).children('.title')[0];
		editor = makeDefinitionEditor(editorUI,
			makeChunkStream([''], null, null, null),
			{'onExpand': options.onExpand,
			'onRetract': options.onRetract});
		
		options.load.take(load);
		options.save.take(save);
		options.focus.take(focus);
	};
	reset();
	return chunkStreamEditor;
};