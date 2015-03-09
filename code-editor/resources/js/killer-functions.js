/**
* Return the string obtained by trimming all whitespace
* characters from the given string.
*/
function trimWhiteSpace(str) {
	var start=0, end, pos, c;
	
	end = str.length;
	
	// Get the real start index of the string
	pos = -1;
	c = ' ';
	while(/\s/.test(c)) {
		pos++;
		c = str.charAt(pos);
	}
	start = pos;
	
	// Get the real end index of the string
	pos = end;
	c = ' ';
	while(/\s/.test(c)) {
		pos--;
		c = str.charAt(pos);
	}
	end = pos;
	
	return str.slice(start,end+1);
}

/**
* Trim any whitespace character of the end of the given array,
* stopping it when 2 newlines have been removed.
*/
function trimRightWhiteSpace(array) {
	var result = [], newline = 0, i,c;
	
	for(i=array.length-1; i>=0; i--) {
		c = array[i];
		
		if(/\s/.test(c) && newline < 2 && result.length === 0) {
			if(/\n/.test(c)) {
				newline++;
			}
		} else {
			result.push(c);
		}
	}
	
	return result.reverse();
}

//==================
// IDE UTILITIES
//==================
/**
* Return the type, id, topAction of the given chunk as JSON.
* Returns {"type": type, "id": id, "topAction": topAction, "doc": doc}
* With type one of "fact", "usages", "function", "macro", "variable", "parameter" or "constant".
*
* Except for type "fact", id is the name of the function/macro/variable/parameter/constant.
* For a fact, it is the whole code itself, except for atom where it is null.
*/
function formType(code) {
	var codeChunk = trimWhiteSpace(code), chunkLength = codeChunk.length, type = "fact", id = null, topAction = null, idx, idx2;
	var goToNextMeaningfulChar = function() {
		while(!(/\s/.test(codeChunk.charAt(idx))) && idx < chunkLength) {
			idx++;
		}
	};
	// Find where the first WS character is
	// to capture the name of the function used
	var captureName = function() {
		var oldIdx = idx, character, inString = false;
		
		character = codeChunk.charAt(idx);
		if(character === '"') {
			inString = !inString;
		}
		
		while((inString || !(/\s/.test(character) || ')' === character))
				&& idx < chunkLength) {
			idx++;
			character = codeChunk.charAt(idx);
			if(character === '"') {
				inString = !inString;
			}
		}
		return codeChunk.slice(oldIdx,idx);
	};
	
	if('(' === codeChunk.charAt(0)) {
		idx = 1;
		topAction = captureName();
		
		// Get the type
		if(topAction === "defun") {
			type = "function";
		} else if(topAction === "defmacro") {
			type = "macro";
		} else if(topAction === "defvar") {
			type = "variable";
		} else if(topAction === "defparameter") {
			type = "parameter";
		} else if(topAction === "defconstant") {
			type = "constant";
		} else if(topAction === "defsetf") {
			type = "simple-setf";
		} else if(topAction === "def-access") {
			type = "access";
		} else if(/".*"/.test(topAction)) {
			type = "usage";
		}
		
		// Get the id
		if(type !== "fact" && type !== 'usage') {
			idx++;
			idx2 = idx;
			goToNextMeaningfulChar();
			id = codeChunk.slice(idx2,idx);
		}
	}
	
	// Get the id for fact or usage forms (atom are excluded)
	if('(' === codeChunk.charAt(0)) {
		if(type === "fact") {
			id = codeChunk;
		} else if(type === "usage") {
			id = topAction;
		}
	}
	
	return {'type': type, 'id': id};
}

function isActionDefinition(chunk) {
	return chunk && (chunk.type === 'function' || chunk.type === 'macro');
}

function makeChunk(code, wip, systemName, moduleName, actionName) {
	var info = formType(code);
	var chunk = {};
	
	chunk.code = code;
	chunk.id = info.id;
	chunk.type = info.type;
	chunk.wip = wip;
	chunk.actionName = info.type === 'fact' || info.type === 'usage' ? actionName : info.id;
	chunk.moduleName = moduleName;
	chunk.systemName = systemName;
	
	return chunk;
}

function makeChunkKey(chunk) {
	var chunkKey = {};
	
	chunkKey.code = '';
	chunkKey.id = chunk.id;
	chunkKey.type = chunk.type;
	chunkKey.actionName = chunk.actionName;
	chunkKey.moduleName = chunk.moduleName;
	chunkKey.systemName = chunk.systemName;
	
	return chunkKey;
}

function makeChunkStream(chunks, systemName, moduleName, actionName) {
	var chunkStream = {};
	
	chunkStream.code = myMap(chunks, function(chunk) { return chunk.code; }).join('\n\n');
	chunkStream.id = null;
	chunkStream.type = 'stream';
	chunkStream.wip = false;
	chunkStream.actionName = actionName;
	chunkStream.moduleName = moduleName;
	chunkStream.systemName = systemName;
	
	return chunkStream;
}

var positionOfChunk = function(chunk, chunks) {
	var i = 0, pos = -1;
	while(i<chunks.length && pos === -1) {
		if(chunk.id === (chunks[i]).id && chunk.type === (chunks[i]).type) {
			pos = i;
		}
		i++;
	}
	return pos;
};

/**
* Identify chunks in all the content of the editor.
*/
function identifyChunks(initContents, systemName, moduleName, actionName) {
	var contents = '', resultChunks = [], wip = [], chunk = [];
	var parensCount = 0, state = 'code';
	var i,c,prevC = '', newline = false, atom = '';
	
	var locateChunks = function(chunks, wips) {
		var i, result = [];
		for(i=0; i<chunks.length; i++) {
			result.push(makeChunk(chunks[i], wips[i], systemName, moduleName, actionName));
		}
		return result;
	};
	
	/*
	* State default is 'code', meaning we store characters because
	* they are part of chunk.
	* 
	* State can be 'string', 'comment', 'areaComment',
	* 'special', 'unprintable' or 'codeChunk'.
	* The first 5 activate a capture of characters that
	* ignores closing parens ('special' denotes syntax
	* functionning with read-macros).
	*
	* The first 4 are part of a code chunk so when leaving
	* these states we go back to default capture.
	* But with 'unprintable', the chunk capture happens
	* immediately.
	*
	* The state come back to code with a certain character :
	* " for 'string' (which also activate the 'string' state)
	* \n for 'comment' (activated by ;)
	* | then # for 'areaComment' (activated by #|)
	* \s for 'special'
	* > for 'unprintable'
	*
	* The last state, 'codeChunk', denotes that a complete
	* chunk of code has been captured. All successive whitespace
	* characters are gobbled up until a non-whitespace one is
	* encoutered : 2 \n are then appended for the editor.
	* This ensures that chunks are *exactly* separated by 1 blank
	* line.
	*/
	for(i=0; i< initContents.length; i++) {
		c = initContents.charAt(i);
		
		if(state === 'codeChunk') {
			if(!(/\s/.test(c))) {
				atom = '';
				contents += '\n\n';
				state = 'code';
			}
		}
		
		if(state !== 'codeChunk') {
			chunk.push(c);
			
			// Ordered conditions : first check to go out of a special capture state
			// Then check to enter a special capture state
			// Else we are in the default state so count parens
			// Special case : parens count is 0, a whitespace character is encountered
			// and some non-whitespace characters have been captured -> it is an atom
			if(state === 'string') {
				if(c === '"') state = 'code';
				
			} else if(state === 'comment') {
				if(/\n/.test(c)) state = 'code';
				
			} else if(state === 'areaComment') {
				if(c === '#' && prevC === '|') state = 'code';
				
			} else if(state === 'readerMacro') {
				if(/\s/.test(c)) {
					state = 'code';
				} else if(c === ')') {
					parensCount--;
					state = 'code';
				} else if (c === '(') {// reader macro #' with a lamdba form
					parensCount++;
					state = 'code';
				}
			
			} else if(state === 'unprintable') {
				if(c === '>') {
					contents += chunk.join('');
					wip.push(false);
					resultChunks.push(chunk.join(''));
					chunk = [];
					state = 'codeChunk';
				}
				
			} else if(c === '"') {
				state = 'string';
				
			} else if(c === ';') {
				state = 'comment';
				
			} else if(prevC === '#') {
				if(c === '|') {
					state = 'areaComment';
				} else if(c === '<') {
					state = 'unprintable';
				} else {
					state = 'readerMacro';
				}
				
			} else if(parensCount === 0 && /\s/.test(c)) {
				atom = trimWhiteSpace(chunk.join(''));
				if(atom !== '') {
					contents += atom;
					wip.push(false);
					resultChunks.push(atom);
					chunk = [];
					state = 'codeChunk';
				}
				
			} else if(c === '(') {
				parensCount++;
				
			} else if(c === ')') {
				parensCount--;
				if(parensCount === 0) {
					contents += chunk.join('');
					wip.push(false);
					resultChunks.push(chunk.join(''));
					chunk = [];
					state = 'codeChunk';
				}
			}
			
			if(/\n/.test(c) && state !== 'string') {
				if(atom === '') {
					// check for a blank line delimiter. If it is, flag it or, if the flag is
					// already set, it is an unfinished chunk so save it and reset parensCount
					// (we use a flag because we cannot check prevC as formatting insert tabs
					// and/or space...)
					// Also :
					// we need to remove 2 newline characters along with any other whitespace
					// characters inserted by formatting.
					if(newline) {
						parensCount = 0;
						newline = false;
						
						contents += trimRightWhiteSpace(chunk).join('');
						wip.push(true);
						resultChunks.push(trimRightWhiteSpace(chunk).join(''));
						chunk = [];
						state = 'codeChunk';
					} else {
						newline = true;
					}
				}
			} else if(!(/\s/.test(c))) {
				// if c is not any whitespace character, turn off the newline flag.
				newline = false;
			}
		}
		
		prevC = c;
	}
	
	// <=> a chunk is unfinished >somewhere<
	// Because it may be that a blank line has not been inserted to clearly delimite it.
	// This means that it may not just be the last chunk that is unfinished.
	// So put everything back in the contents
	if(parensCount !== 0) {
		contents += chunk.join('');
		wip.push(true);
		resultChunks.push(chunk.join(''));
	} else {
		// we may have an atom or the empty code!
		atom = trimWhiteSpace(chunk.join(''));
		if(atom !== '') {
			wip.push(false);
			contents += atom;
			contents += '\n\n';
			resultChunks.push(atom);
		}
	}
	
	return locateChunks(resultChunks, wip);
}
