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
* 1. Insert some code with setValue and validate getValue. [6]
*    Validate onCodeModified is fired in appropriate situations: fired when some modifications
*    happens but not fired again on further modifications.
*    For more involved cases, no tests => EC tests must all be OK because EA can't be expected
*    to work as required if not.
*
* 2. The inserted code contains several chunks so validate getCodeChunk (including bad indexes)
* and getAllCodeChunks (these chunks include a complete chunk, a wip chunk, and atoms and
* unprintable objects separated by 2 blank lines or by a space or by nothing). [18]
*
* 3. Round 2 : validate another call to setValue with less code chunks. [11]
*
* 4. Validate save and load with WIP and complete chunks. [6]
*
* 5. By hooking into the editable chunks, provoke the onNewChunksDefined and validate. [29]
*
* 6. Round 2 for onNewChunksDefined : hook on an EC created with the onNewChunksDefined situation. [16]
*
* 7. Validate that onChunkCompletion is appropriately fired. [8]
*
* 8. Validate the clear method and its appropriate consequences on getters. [4]
*
* Note: format doesn't work for now but chunk identification will probably be needed here too
* when its format will do its stuff.
*/
var total = 72;
plan({ tests: total });

var tmp, moduleName, systemName;

tmp = clearDraftSystem();
systemName = tmp['system-name'];
moduleName = tmp['module-name'];

if(!(systemName && moduleName)) {
	skip('Clear draft system failed.',total);
} else {
	// Event checking utilities
	var evtChunkCompletion, evtCodeModified;
	var resetEvtFlags = function() {
		evtChunkCompletion = [];
		evtCodeModified = false;
	};
	var flagChunkCompletion = function(completedCode) {
		evtChunkCompletion.push(completedCode);
	};
	var flagCodeModified = function(flag) {
		evtCodeModified = flag;
	};
	var onCodeModifiedFired = function() {
		return evtCodeModified;
	};
	
	// First, prepare code chunks
	var codeFact = '(foo-fun 2)', factAction = 'foo-fun';
	var simpleCodeFact = '(foo-fun)', simpleFactAction = 'foo-fun';
	var codeAtomFact = '5', atomAction = null;
	var codeUnprintableObject = '#<The non-printable object>';
	var codeFun = '(defun foo-fun (x) (+ x 2))', codeFun2 = '(defun foo-fun (x) (+ x 3))', preCodeFun = '(defun foo-fun ', postCodeFun = '(x) (+ x 1))', codeDocFun = '(defun foo2-fun (x) "This function is documented" (- x 2))', codeLargeDocFun = '(defun foo3-fun (x) "This function is'+'\n\n'+'documented" (- x 2))', funAction = 'defun';
	var codeMac = '(defmacro foo-mac () \'(foo 2))', codeMac2 = '(defmacro foo-mac () \'(foo 7))', preCodeMac = '(defmacro foo-mac', codeDocMac = '(defmacro foo2-mac () "This macro is documented" \'(foo2-fun 10))', macAction = 'defmacro';
	var codeVar = '(defvar foo-var 5)', codeVar2 = '(defvar foo-var 6)', codeDocVar = '(defvar foo2-var 7 "This variable is documented")', varAction = 'defvar';
	var codeParam = '(defparameter foo-param 2)', codeParam2 = '(defparameter foo-param 3)', codeDocParam = '(defparameter foo2-param 4 "This parameter is documented")', paramAction = 'defparameter';
	var codeConst = '(defconstant foo-const "toto")', codeConst2 = '(defconstant foo-const "tutu")', codeDocConst = '(defconstant foo2-const "titi" "This constant is documented")', constAction = 'defconstant';

	// Make an editing area
	$("body").append('<div id="editingArea"></div>');
	var eaTestContainer = $('#editingArea'), code, formattedCode, loadedCode, editor, ec;
	resetEvtFlags();
	
	var installEA = function() {
		eaTestContainer.empty();
		resetEvtFlags();
		d();
		editor = makeEditingArea(document.getElementById('editingArea'),
			{'onChunkCompletion': flagChunkCompletion,
			 'onCodeModified': flagCodeModified});
	};
	
	var valueTest = function(editor,values) {
		var i;
		for(i=0; i<values.length; i++) {
			cmpOK(editor.getCodeChunk(i).code, '===', values[i], 'Chunk n°'+i);
		}
	};
	
	var synchroTest = function(editor,chunks,expectedNb) {
		var i;
		for(i=0; i<expectedNb; i++) {
			isDeeply(editor.getCodeChunk(i).code, chunks[i], 'Synchro OK getChunk and getAll ('+i+')');
		}
	};
	
	var boundTest = function(editor,upper) {
		cmpOK(editor.getCodeChunk(-1),'===',null,'getCodeChunk null when index is negative');
		cmpOK(editor.getCodeChunk(upper),'===',null,'getCodeChunk null when index is out of the upper bound');
	};
	
	ddiag('/** GET AND SET VALUE **/');
	installEA();
	ec = editor.getEC(0);
	cmpOK(editor.getValue(),'===','','Editor is empty on init');
	
	tmp = [];
	code = '';
	var gsValHelper = function (value,separator) {
		code += value;
		code += separator;
		tmp.push(value);
	};
	
	gsValHelper(codeFun, ' ');
	gsValHelper(preCodeMac, '\n\n\n');
	gsValHelper(codeUnprintableObject, ' ');
	gsValHelper(codeAtomFact, '\n\n');
	gsValHelper(codeAtomFact, ' ');
	gsValHelper(codeUnprintableObject, '');
	tmp = tmp.join('\n\n');
	
	editor.setValue(code, moduleName, systemName);
	cmpOK(editor.getValue(),'===',tmp,'Set and get value OK');
	
	installEA();
	ec = editor.getEC(0);
	editor.setValue('', moduleName, systemName);
	ok(!onCodeModifiedFired(),'onCodeModified was not fired on init');
	
	ec.setCode({'code': code, 'moduleName': moduleName, 'systemName': systemName});
	ok(onCodeModifiedFired(),'onCodeModified was fired on modifying code');
	
	resetEvtFlags();
	ec.setCode({'code': '', 'moduleName': moduleName, 'systemName': systemName});
	ok(!onCodeModifiedFired(),'onCodeModified was not fired when modifying the code again');
	
	editor.save();
	resetEvtFlags();
	ec.setCode({'code': codeMac, 'moduleName': moduleName, 'systemName': systemName},true);
	ok(onCodeModifiedFired(),'onCodeModified was fired on modifying a code that was just saved');
	
	
	ddiag('/** GET CODE CHUNK(S) **/');
	installEA();
	editor.setValue(code, moduleName, systemName);
	tmp = editor.getAllCodeChunks();
	if(tmp.length !== 6) {
		skip('6 chunks were expected but '+tmp.length+' editor chunks present. Editor value \n'+editor.getValue(),14);
	} else {
		valueTest(editor,[codeFun, preCodeMac, codeUnprintableObject, codeAtomFact, codeAtomFact, codeUnprintableObject]);
		synchroTest(editor,tmp,6);
		boundTest(editor,6);
	}
	
	editor.setValue('', moduleName, systemName);
	tmp = editor.getAllCodeChunks();
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' editor chunks present. Editor value \n'+editor.getValue(),5);
	} else {
		valueTest(editor,['']);
		synchroTest(editor,tmp,1);
		boundTest(editor,1);
	}
	
	
	ddiag('/** ROUND 2 **/');
	installEA();
	tmp = [];
	code = '';
	
	gsValHelper(codeMac, '\n');
	gsValHelper(preCodeFun, '\n\n\n');
	gsValHelper(codeVar, '\n');
	gsValHelper(preCodeMac, '');
	tmp = tmp.join('\n\n');
	editor.setValue(code, moduleName, systemName);
	
	cmpOK(editor.getValue(),'===',tmp,'With a new value, set and get value OK');
	
	tmp = editor.getAllCodeChunks();
	if(tmp.length !== 4) {
		skip('4 chunks were expected but '+tmp.length+' editor chunks present. Editor value \n'+editor.getValue(),44);
	} else {
		valueTest(editor,[codeMac, preCodeFun, codeVar, preCodeMac]);
		synchroTest(editor,tmp,4);
		boundTest(editor,4);
		
		
		ddiag('/** BASIC SAVE & LOAD **/');
		clearDraftSystem();
		installEA();
		editor.setValue(codeFun+'\n\n'+codeFun+'\n\n'+codeFun+'\n\n'+codeFun, moduleName, systemName);
		// really modify EC themselves to allow saving
		editor.getEC(0).setCode({'code': codeMac, 'moduleName': moduleName, 'systemName': systemName});
		editor.getEC(1).setCode({'code': preCodeFun, 'moduleName': moduleName, 'systemName': systemName});
		editor.getEC(2).setCode({'code': codeVar, 'moduleName': moduleName, 'systemName': systemName});
		editor.getEC(3).setCode({'code': preCodeMac, 'moduleName': moduleName, 'systemName': systemName});
		editor.save();
		cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun+'\n\n'+preCodeMac,'All WIP chunks were saved and the order was preserved');
		cmpOK(loadChunks(moduleName,systemName),'===',codeMac+'\n\n'+codeVar+'\n\n','All complete chunks were saved and the order was preserved');
		
		tmp = '(defun bar-baz (toto';
		editor.getEC(1).setCode({'code': tmp,'moduleName': moduleName, 'systemName': systemName});
		editor.save();
		cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeMac+'\n\n'+tmp,'New WIP chunk -> is saved and previous one is deleted');
		cmpOK(loadChunks(moduleName,systemName),'===',codeMac+'\n\n'+codeVar+'\n\n','Module code is unchanged');
		
		editor.getEC(1).setCode({'code': codeFun,'moduleName': moduleName, 'systemName': systemName});
		editor.save();
		cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeMac,'Completed chunk -> previously saved WIP chunk is deleted');
		cmpOK(loadChunks(moduleName,systemName),'===',codeMac+'\n\n'+codeVar+'\n\n'+codeFun+'\n\n','Newly completed chunk is saved');
		
		ddiag('/** HOOKING INTO EC : onNewChunksDefined **/');
		editor.getEC(1).resetCode({'code': preCodeFun,'moduleName': moduleName, 'systemName': systemName});
		ec = editor.getEC(0);
		ec.setCode({'code': codeMac+' '+codeParam+'\n'+codeParam2+'\n\n', 'moduleName': moduleName, 'systemName': systemName});
		tmp = [codeMac, codeParam, codeParam2, preCodeFun, codeVar, preCodeMac];
		
		cmpOK(editor.getValue(),'===',tmp.join('\n\n'),'Get value OK');
		
		tmp = editor.getAllCodeChunks();
		if(tmp.length !== 6) {
			skip('6 chunks were expected but '+tmp.length+' editor chunks present. Editor value \n'+editor.getValue(),29);
		} else {
			valueTest(editor,[codeMac, codeParam, codeParam2, preCodeFun, codeVar, preCodeMac]);
			synchroTest(editor,tmp,6);
			boundTest(editor,6);
			
			
			ddiag('/** ROUND 2 onNewChunksDefined **/');
			ec = editor.getEC(1);
			ec.setCode({'code': codeParam+'\n\n\n'+codeConst, 'moduleName': moduleName, 'systemName': systemName});
			tmp = [codeMac, codeParam, codeConst, codeParam2, preCodeFun, codeVar, preCodeMac];
			
			cmpOK(editor.getValue(),'===',tmp.join('\n\n'),'Get value OK');
			
			tmp = editor.getAllCodeChunks();
			if(tmp.length !== 7) {
				skip('7 chunks were expected but '+tmp.length+' editor chunks present. Editor value \n'+editor.getValue(),16);
			} else {
				valueTest(editor,[codeMac, codeParam, codeConst, codeParam2, preCodeFun, codeVar, preCodeMac]);
				synchroTest(editor,tmp,7);
				boundTest(editor,7);
			}
			
			
			ddiag('/** SAVE & LOAD with several chunks in EC **/');
			clearDraftSystem();
			editor.setValue('', moduleName, systemName);
			
			code = '';
			gsValHelper(codeMac, '\n');
			gsValHelper(preCodeFun, '\n\n\n');
			gsValHelper(codeVar, '\n');
			gsValHelper(preCodeMac, '');
			editor.getEC(0).setValue(code);
			
			if(editor.getEC(1) !== null) {
				skip('Expected only one EC',2);
			} else {
				tmp = loadWipChunk(moduleName,systemName);
				if(tmp !== '') {
					skip('Expected no saved WIP chunks after clearing the draft system but got '+tmp,2);
				} else {
					tmp = loadWipChunk(moduleName,systemName);
					if(tmp !== '') {
						skip('Expected no saved complete chunks after clearing the draft system but got '+tmp,2);
					} else {
						editor.save();
						cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun+'\n\n'+preCodeMac,'All WIP chunks of one EC were saved and the order was preserved');
						cmpOK(loadChunks(moduleName,systemName),'===',codeMac+'\n\n'+codeVar+'\n\n','All complete chunks of one EC were saved and the order was preserved');
					}
				}
			}
		}
		
		
		ddiag('/** FIRING onChunkCompletion **/');
		code = codeMac;
		code += preCodeFun;
		code += '\n\n\n';
		code += codeVar;
		resetEvtFlags();
		editor.setValue(code, moduleName, systemName);
		
		if(evtChunkCompletion.length != 2) {
			skip('Problem with firing onChunkCompletion. Waited 2 code chunks but got '+evtChunkCompletion.length+'. Details of values \n'+JSON.stringify(evtChunkCompletion),8);
		} else {
			cmpOK(evtChunkCompletion[0].code, '===', codeMac, 'Complete chunk n°1');
			cmpOK(evtChunkCompletion[1].code, '===', codeVar, 'Complete chunk n°2');
			
			resetEvtFlags();
			ec = editor.getEC(1);
			ec.setCode({'code': preCodeFun + postCodeFun, 'moduleName': moduleName, 'systemName': systemName});
			
			if(evtChunkCompletion.length != 1) {
				skip('Problem with firing onChunkCompletion on completion of a WIP chunk. Details of values \n'+JSON.stringify(evtChunkCompletion),1);
			} else {
				cmpOK(evtChunkCompletion[0].code, '===', preCodeFun + postCodeFun, 'Completion of WIP chunk');
			}
			
			resetEvtFlags();
			ec = editor.getEC(0);
			ec.setCode({'code': codeMac+codeConst+'\n\n'+codeFun, 'moduleName': moduleName, 'systemName': systemName});
			
			if(evtChunkCompletion.length != 3) {
				skip('Problem with firing onChunkCompletion when adding several chunks to an existing EC. Waited 3 code chunks but got '+evtChunkCompletion.length+'. Details of values \n'+JSON.stringify(evtChunkCompletion),3);
			} else {
				cmpOK(evtChunkCompletion[0].code, '===', codeMac, 'Complete chunk n°1');
				cmpOK(evtChunkCompletion[1].code, '===', codeConst, 'Complete chunk n°2');
				cmpOK(evtChunkCompletion[2].code, '===', codeFun, 'Complete chunk n°3');
			}
			
			resetEvtFlags();
			ec = editor.getEC(1);
			ec.setCode({'code': codeConst+'\n'+codeParam, 'moduleName': moduleName, 'systemName': systemName});
			
			if(evtChunkCompletion.length != 2) {
				skip('Problem with firing onChunkCompletion on an EC created through onNewChunksDefined. Waited 2 code chunks but got '+evtChunkCompletion.length+'. Details of values \n'+JSON.stringify(evtChunkCompletion),2);
			} else {
				cmpOK(evtChunkCompletion[0].code, '===', codeConst, 'Complete chunk n°1');
				cmpOK(evtChunkCompletion[1].code, '===', codeParam, 'Complete chunk n°2');
			}
		}
	}
	
	
	ddiag('/** CLEAR METHOD **/');
	editor.clear();
	cmpOK(editor.getValue(),'===','','Clear OK on getValue');
	cmpOK(editor.getCodeChunk(0).code,'===','','Clear OK on getCodeChunk');
	cmpOK(editor.getAllCodeChunks().length,'===',1,'Clear OK on getAllCodeChunks length') || diag('Editor all chunks '+JSON.stringify(editor.getAllCodeChunks()));
	cmpOK(editor.getAllCodeChunks()[0].code,'===','','Clear OK on getAllCodeChunks content') || diag('Editor all chunks '+JSON.stringify(editor.getAllCodeChunks()));
}
