//==============
// SYSTEM
//==============
/**
* Clear the draft system.
* Return true if all went well, false otherwise.
*/
function clearDraftSystem() {
	var result = true;
	$.ajax({'url':'/code-editor/web-clear-draft-system',
			'async': false,
			'type': 'POST',
			'success': function(data) {
				try {
					result = JSON.parse(data);
				} catch(e) {
					console.error('clearDraftSystem: '+ e);
				}
			},
			'error': function(request,status,error) {
				result = false;
				console.error('clearDraftSystem:' + status || error);
			}});
	return result;
}

//==============
// GENERAL CODE
//==============

/* Assume all chunks have the same moduleName and systemName */
function saveCompleteChunks(chunks, moduleName, systemName) {
	var result = null;
	$.ajax({'url': '/code-editor/web-save-chunks',
			'async': false,
			'type': 'POST',
			'data': {'chunks': chunks,
						'module-name': moduleName,
						'system-name': systemName},
			'success': function(data) {
				try {
					result = JSON.parse(data);
				} catch(e) {
					console.error('saveCompleteChunks: Problem ('+e+') when parsing data: '+data);
				}
			},
			'error': function(request,status,error) {
				console.error('saveCompleteChunks: '+ status || error);
			}});
	return result;
}

/* Assume all chunks have the same moduleName and systemName */
function saveWipChunks(chunks, moduleName, systemName) {
	var result = null;
	$.ajax({'url': '/code-editor/web-save-wip',
			'async': false,
			'type': 'POST',
			'data': {'wip': chunks,
					'module-name': moduleName,
					'system-name': systemName},
			'success': function(data) {
				try {
					result = JSON.parse(data);
				} catch(e) {
					console.error('saveWipChunks: Problem ('+e+') when parsing data: '+data);
				}
			},
			'error': function(request,status,error) {
				console.error('saveWipChunks: '+ status || error);
			}});
	return result;
}

/* Assume all expectations have the same moduleName and systemName */
function saveExpectations(expectations) {
	var result = null;
	if($.isArray(expectations) && expectations.length > 0) {
		$.ajax({'url': '/code-editor/web-save-expectations',
				'async': false,
				'type': 'POST',
				'data': {'str': packToString(expectations, 'code'),
						'ids': packToString(expectations, 'id'),
						'module-name': expectations[0].moduleName,
						'system-name': expectations[0].systemName},
				'success': function(data) {
					try {
						result = JSON.parse(data);
					} catch(e) {
						console.error('saveExpectation: Problem ('+e+') when parsing data: '+data);
					}
				},
				'error': function(request,status,error) {
					console.error('saveExpectation: '+ status || error);
				}});
	}
	return result;
}

function deleteSavedWipChunk(wipChunk, moduleName, systemName, async, successCB) {
	var result = null, realAsync = async ? async : false;
	if(wipChunk !== '') {
		$.ajax({'url': '/code-editor/web-delete-saved-wip-chunk',
				'async': realAsync,
				'type': 'POST',
				'data': {'wipChunk': wipChunk, 'module-name': moduleName, 'system-name': systemName},
				'success': function(data) {
					try {
						result = JSON.parse(data);
					} catch(e) {
						console.error('deleteSavedWipChunk: Problem ('+e+') when parsing data: '+data);
					}
					if(successCB && typeof successCB === 'function') {
						successCB.apply(null,[wipChunk,result]);
					}
				},
				'error': function(request,status,error) {
					console.error('deleteSavedWipChunk-errorStatus: '+ status);
					console.error('deleteSavedWipChunk-errorMsg: '+ error);
				}});
	}
	return result;
}

function loadWipChunk(moduleName, systemName) {
	var result = '';
	$.ajax({'url': '/code-editor/web-load-wip',
			'async': false,
			'type': 'POST',
			'data': {'module-name': moduleName, 'system-name': systemName},
			'success': function(data) {
				result = data;
			},
			'error': function(request,status,error) {
				console.error('loadWIPCode: '+ status || error);
			}});
	return result;
}

/**
* Save the given fact.
* Return the fact that have been saved by the server, null if something failed.
*/
function saveFact(fact, coordinates, async, successCB, errorCB) {
	var result = null, realAsync = async ? async : false;
	$.ajax({'url': '/code-editor/web-save-fact',
			'async': realAsync,
			'type': 'POST',
			'data': {'fact': fact, 'id': coordinates.topAction, 'system-name': coordinates.systemName},
			'success': function(data) {
				try {
					result = JSON.parse(data);
				} catch(e) {
					console.error('saveFact: Problem ('+e+') when parsing data: '+data);
				}
				if(successCB && typeof successCB === 'function') {
					successCB.apply(null,[fact,result]);
				}
			},
			'error': function(request,status,error) {
				console.error('saveFact: '+ status || error);
				if(errorCB && typeof errorCB === 'function') {
					errorCB.apply(null,[fact,result]);
				}
			}});
	return result;
}