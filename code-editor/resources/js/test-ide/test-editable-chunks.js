function ddiag(str) {
	diag('');
	diag(str);
}
function d() {
	diag('');
}
/**
* == TEST PLAN ==
* ===============
*
* Theoretically, there are really 3 usages cases to code here about EC:
* appropriate state & behavior at init, appropriate s&b with using setCode
* and appropriate s&b with modification in the text area itself.
*
* But I failed to modify programmatically the text area and still make the right
* event fired (using setValue of CodeMirror does not fire the onChange event,
* neither does modifying the textarea that CM wraps).
*
* It's a shame as it is the most important usage of the EC...
*
* So, ##implementation knowledge##, the onModif method is practically equal
* to the inner method of analyzing code, method on which the EC is based upon for
* everything.
* We just have to trust CM for working as it should. And trust our approach (described
* below, step 3) is sound.
* 
* -- EDITABLE CHUNKS -- [0]
* 1. Validate getChunk and setCode [4]
* 2. Modification usages [22]
* 3. Complete analyze of s&b, operating as follows: [90]
*    - EC creation
*    - validate state (isWorkInProgress) & behavior (events)
*    - reset the content then set it back to the initial value
*    - validate saving is ok in both WIP and Complete chunk cases
*    - validate s&b are the same
*/
plan({ tests: 81 });

$('body').append('<div id="ecTestContainer"></div>');
var ecTestContainer = $('#ecTestContainer'), ec, tmp, code;

var moduleName, systemName;

tmp = clearDraftSystem();
systemName = tmp['system-name'];
moduleName = tmp['module-name'];

var evtNewChunksDefined, evtChunkCompletion, evtCodeModified;
var resetEvtFlags = function() {
	evtNewChunksDefined = [];
	evtChunkCompletion = [];
	evtCodeModified = null;
};
var flagNewChunksDefined = function(chunks) {
	evtNewChunksDefined.push(chunks);
};
var onNewChunksDefinedFired = function() {
	return evtNewChunksDefined.length != 0;
};
var flagChunkCompletion = function(completedCode) {
	evtChunkCompletion.push(completedCode);
};
var onChunkCompletionFired = function() {
	return !(evtChunkCompletion.length == 0);
};
var flagCodeModified = function(chunk) {
	evtCodeModified = chunk;
};
var onCodeModifiedFired = function() {
	return evtCodeModified !== null;
};

var installEC = function(codeChunk) {
	ecTestContainer.empty();
	resetEvtFlags();
	d();
	ec = makeEditableChunk(ecTestContainer.get(0),
			{'onNewChunksDefined': flagNewChunksDefined,
			'onChunkCompletion': flagChunkCompletion,
			'onCodeModified': flagCodeModified,
			'initChunk': codeChunk});
};

var simpleInstall = function(val) {
	installEC({'code': val, 'moduleName': moduleName, 'systemName': systemName});
};

var simpleSetCode = function(val) {
	ec.setCode({'code': val, 'moduleName': moduleName, 'systemName': systemName});
};

var simpleTestChunk = function(chunk, val, wip, msg) {
	isDeeply({'code': chunk.code, 'wip': chunk.wip, 'moduleName': chunk.moduleName,
			'systemName': chunk.systemName},
			{'code': val, 'wip': wip, 'moduleName': moduleName, 'systemName': systemName},
			msg);
};

// Either fired or not. If fired, always fired once and the appropriate chunk is returned
var validateOnChunkCompletion = function(prefixMsg, isFired, expectedCode) {
	if(!isFired) {
		ok(!onChunkCompletionFired(),prefixMsg+' onChunkCompletion not fired');
	} else {
		ok(onChunkCompletionFired(),prefixMsg+' onChunkCompletion fired');
		
		if(evtChunkCompletion.length != 1) {
			skip(prefixMsg+' onChunkCompletion has not been fired one time but '+evtChunkCompletion.length+'; Details of value :\n'+JSON.stringify(evtChunkCompletion),1);
		} else {
			simpleTestChunk(evtChunkCompletion[0], expectedCode, false, prefixMsg+' onChunkCompletion argument OK');
		}
	}
};

// Either fired or not. If fired, always fired once and the appropriate chunk are returned
var validateOnNewChunksDefined = function(prefixMsg, isFired, expectedCodes, expectedWIPflags) {
	var i;
	if(!isFired) {
		ok(!onNewChunksDefinedFired(),prefixMsg+' onNewChunksDefined not fired');
	} else {
		ok(onNewChunksDefinedFired(),prefixMsg+' onNewChunksDefined fired');
		
		if(evtNewChunksDefined.length != 1) {
			skip('onNewChunksDefined has not been fired one time but '+evtNewChunksDefined.length+'. Details of value :\n'+JSON.stringify(evtNewChunksDefined),expectedCodes.length);
		} else {
			for(i=0; i<expectedCodes.length; i++) {
				simpleTestChunk((evtNewChunksDefined[0])[i], expectedCodes[i], expectedWIPflags[i], prefixMsg+' New chunk n°'+i+' OK');
			}
		}
	}
};

// Validate at installation and via setCode: the results must be the same no matter what.
var validateECchunkDetection = function(prefixMsg, code, chunkCompletionFired, wipFlags, resetCode) {
	var i, tmp, codeToInstall = code.join(''), firstChunk = code[0], restChunks = [];
	var newChunksDefinedFired = wipFlags ? true : false;
	var realResetCode = resetCode ? resetCode : '';
	
	tmp = code.slice(2);
	for(i=0; i<tmp.length; i++) {
		if(i % 2 === 0) {
			restChunks.push(tmp[i]);
		}
	}
	
	// Test procedure
	simpleInstall(codeToInstall);
	
	if(chunkCompletionFired) {
		ok(!ec.isWorkInProgress(),prefixMsg+'[init] First chunk is complete');
		validateOnChunkCompletion(prefixMsg+'[init]',true,firstChunk);
	} else {
		ok(ec.isWorkInProgress(),prefixMsg+'[init] First chunk is work-in-progress');
		validateOnChunkCompletion(prefixMsg+'[init]',false);
	}
	
	if(newChunksDefinedFired) {
		validateOnNewChunksDefined(prefixMsg+'[init]',true,restChunks,wipFlags);
	} else {
		validateOnNewChunksDefined(prefixMsg+'[init]',false);
	}
	
	simpleSetCode(realResetCode);
	resetEvtFlags();
	simpleSetCode(codeToInstall);
	
	if(chunkCompletionFired) {
		ok(!ec.isWorkInProgress(),prefixMsg+' First chunk is complete');
		validateOnChunkCompletion(prefixMsg,true,firstChunk);
	} else {
		ok(ec.isWorkInProgress(),prefixMsg+' First chunk is work-in-progress');
		validateOnChunkCompletion(prefixMsg,false);
	}
	
	if(newChunksDefinedFired) {
		validateOnNewChunksDefined(prefixMsg,true,restChunks,wipFlags);
	} else {
		validateOnNewChunksDefined(prefixMsg,false);
	}
};


if(!(systemName && moduleName)) {
	skip('Clear draft system failed.',81);
} else {
	// Same code chunks as for any tests (test-functions, test-editing-area, ...)
	var codeFact = '(foo-fun 2)', factAction = 'foo-fun';
	var simpleCodeFact = '(foo-fun)', simpleFactAction = 'foo-fun';
	var codeAtomFact = '5', atomAction = null;
	var codeUnprintableObject = '#<The non-printable object>';
	var codeFun = '(defun foo-fun (x) (+ x 2))', codeFun2 = '(defun foo-fun (x) (+ x 3))', preCodeFun = '(defun foo-fun ', postCodeFun = '(x) (+ x 1))', codeDocFun = '(defun foo2-fun (x) "This function is documented" (- x 2))', codeLargeDocFun = '(defun foo3-fun (x) "This function is'+'\n\n'+'documented" (- x 2))', funAction = 'defun';
	var codeMac = '(defmacro foo-mac () \'(foo 2))', codeMac2 = '(defmacro foo-mac () \'(foo 7))', preCodeMac = '(defmacro foo-mac', codeDocMac = '(defmacro foo2-mac () "This macro is documented" \'(foo2-fun 10))', macAction = 'defmacro';
	var codeVar = '(defvar foo-var 5)', codeVar2 = '(defvar foo-var 6)', codeDocVar = '(defvar foo2-var 7 "This variable is documented")', varAction = 'defvar';
	var codeParam = '(defparameter foo-param 2)', codeParam2 = '(defparameter foo-param 3)', codeDocParam = '(defparameter foo2-param 4 "This parameter is documented")', paramAction = 'defparameter';
	var codeConst = '(defconstant foo-const "toto")', codeConst2 = '(defconstant foo-const "tutu")', codeDocConst = '(defconstant foo2-const "titi" "This constant is documented")', constAction = 'defconstant';
	
	
	ddiag('/** GET & SET CODE **/');
	simpleInstall('');
	simpleTestChunk(ec.getChunk(), '', true, '[init] Get code OK with empty text');
	
	simpleSetCode(codeFact);
	simpleTestChunk(ec.getChunk(), codeFact, false, 'Get and set code OK on non-empty text');

	simpleInstall(codeFun);
	simpleTestChunk(ec.getChunk(), codeFun, false, '[init] Get code OK with non-empty text');

	simpleSetCode('');
	simpleTestChunk(ec.getChunk(), '', true, 'Get and set code OK with empty text');


	ddiag('/** MODIFICATION USAGES **/');
	simpleInstall('');
	ok(!ec.isModified(),'[init] Empty EC is considered unmodified');
	ok(!onCodeModifiedFired(),'[init] onCodeModified was not fired');
	
	tmp = ec.isModified();
	tmp = !tmp;
	ok(!ec.isModified(),'[init] Empty EC still unmodified');
	
	resetEvtFlags();
	simpleSetCode('');
	ok(!ec.isModified(),'Setting to empty text => still considered unmodified');
	ok(!onCodeModifiedFired(),'onCodeModified was not fired');
	
	resetEvtFlags();
	simpleSetCode(codeFact);
	ok(ec.isModified(),'Setting to non-empty text => EC flagged as modified');
	ok(onCodeModifiedFired(),'onCodeModified was fired');
	simpleTestChunk(evtCodeModified, codeFact, false, 'Returned chunk ok');
	
	resetEvtFlags();
	simpleSetCode(codeFact);
	ok(ec.isModified(),'Setting to the same non-empty text => EC still flagged as modified');
	ok(!onCodeModifiedFired(),'onCodeModified was not fired');
	
	resetEvtFlags();
	simpleSetCode(codeFun);
	ok(ec.isModified(),'Setting to another non-empty text => EC flagged as modified');
	ok(onCodeModifiedFired(),'onCodeModified was fired');
	simpleTestChunk(evtCodeModified, codeFun, false, 'Returned chunk ok');

	simpleInstall(codeFact);
	ok(!ec.isModified(),'[init] Non-empty EC is considered unmodified');
	ok(!onCodeModifiedFired(),'onCodeModified was not fired');
	
	resetEvtFlags();
	simpleSetCode(codeFact);
	ok(!ec.isModified(),'Setting to the same text => still considered unmodified');
	ok(!onCodeModifiedFired(),'onCodeModified was not fired');
	
	resetEvtFlags();
	simpleSetCode(codeFun);
	ok(ec.isModified(),'Setting to another non-empty text => EC flagged as modified');
	ok(onCodeModifiedFired(),'onCodeModified was fired');
	simpleTestChunk(evtCodeModified, codeFun, false, 'Returned chunk ok');

	resetEvtFlags();
	simpleSetCode('');
	ok(ec.isModified(),'Setting to empty text => EC flagged as modified');
	ok(onCodeModifiedFired(),'onCodeModified was fired');
	simpleTestChunk(evtCodeModified, '', true, 'Returned chunk ok');
	
	
	ddiag('/** COMPLETE ANALYSIS **/');
	// WIP (empty code)
	validateECchunkDetection('[empty code]', [''], false, null, 'toto');

	// WIP (1 chunk)
	validateECchunkDetection('[WIP]', [preCodeFun], false);
	
	// Complete (1 chunk)
	validateECchunkDetection('[C]', [codeFun], true);

	// [C,C,WIP]
	validateECchunkDetection('[C,C,WIP]', [codeConst,' ',codeVar,'\n\n\n',preCodeFun], true, [false,true]);
	
	// [C,WIP,C]
	validateECchunkDetection('[C,WIP,C]', [codeConst,' ',preCodeFun,'\n\n\n',codeVar], true, [true,false]);

	// [WIP,WIP,C]
	validateECchunkDetection('[WIP,WIP,C]', [preCodeMac,'\n\n',preCodeFun,'\n\n\n',codeVar], false, [true,false]);
}