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
* -- UTILITIES --
* 1. trimWS [3]
* 2. trimRightWS [2]
*
*
* -- identifyChunks (in place of EDITABLE CHUNKS) -- [32+6]
* 1) 
* When the code in an editable chunk changes, there are 2 things to check.
* First does the code is rightly seen as a work-in-progress (WIP, opposite is completed)?
* Second does the code still contains 1 chunk?
*
* The change could be a pure addition of text, a modification of existing text,
* or a deletion.
*
* The state must be also known before the change so we have 4 possibilities :
* WIP -> WIP, WIP -> C, C -> WIP and C -> C.
*
* Plus the fact we can have one or more chunk after the change. I want to test
* 3 things : no additional chunk introduced, 1 introduced and 3 introduced.
*
* So! 3 types of change, 4 state switches, 3 situations about chunk introduction.
* That gives 36 tests.
*
* #BUT# I haven't succeed at firing the onChange event of CodeMirror with changing the
* content of the textarea and firing an event from it...
*
* So we test these cases : WIP (empty code), WIP (1 chunk), Complete (1 chunk),
* 3 chunks -> [C,C,WIP] and [C,WIP,C] and [WIP,WIP,C] . And these tests are done
* on the function identifyChunks itself.
*
* 2)
* Special tests :
* - identification of atoms and unprintable objects []
* - identification of unfinished chunks (with and without proper blank line
*   delimitation)
*
* -- Save and load operations -- [17]
* 1. saveChunks (save OK, callback working)
* 2. loadModuleChunk
*
*/
// Prepare : plan the number of tests and clear the draft system
var total = 63;
plan({ tests: total });

var code, tmp, tmp2, tmp3;
var moduleName, systemName;

tmp = clearDraftSystem();
systemName = tmp['system-name'];
moduleName = tmp['module-name'];

if(!(systemName && moduleName)) {
	skip('Clear draft system failed.',total);
} else {
	ddiag('/** UTILITIES **/');
	cmpOK(trimWS('  \n  \t  toto'),'===', 'toto', 'Trim left');
	cmpOK(trimWS('toto  \n  \t  '),'===', 'toto', 'Trim right');
	cmpOK(trimWS('  \n  \t  toto  \n  \t  '),'===', 'toto', 'Trim both');
	
	isDeeply(trimRightWS([' ','\n','\t','t','o',' ','\n','\t']), [' ','\n','\t','t','o'], 'Trim array only at right') || diag('Full result is:\n', JSON.stringify(trimRightWS([' ','\n','\t','t','o',' ','\n','\t'])));
	isDeeply(trimRightWS([' ','\n','\t','t','o',' ','\n','\n']), [' ','\n','\t','t','o',' '], 'Trim array only at right, stopping when 2 newlines have been encountered') || diag('Full result is:\n', JSON.stringify(trimRightWS([' ','\n','\t','t','o',' ','\n','\n'])));
	
	ddiag('/** CHUNK IDENTIFICATION  **/');
	var codeFact = '(foo-fun 2)', factAction = 'foo-fun';
	var simpleCodeFact = '(foo-fun)', simpleFactAction = 'foo-fun';
	var codeAtomFact = '5', atomAction = null;
	var codeUnprintableObject = '#<The non-printable object>';
	var codeFun = '(defun foo-fun (x) (+ x 2))', codeFun2 = '(defun foo-fun (x) (+ x 3))', preCodeFun = '(defun foo-fun ', postCodeFun = '(x) (+ x 1))', codeDocFun = '(defun foo2-fun (x) "This function is documented" (- x 2))', codeLargeDocFun = '(defun foo3-fun (x) "This function is'+'\n\n'+'documented" (- x 2))', funAction = 'defun';
	var codeMac = '(defmacro foo-mac () \'(foo 2))', codeMac2 = '(defmacro foo-mac () \'(foo 7))', preCodeMac = '(defmacro foo-mac', codeDocMac = '(defmacro foo2-mac () "This macro is documented" \'(foo2-fun 10))', macAction = 'defmacro';
	var codeVar = '(defvar foo-var 5)', codeVar2 = '(defvar foo-var 6)', codeDocVar = '(defvar foo2-var 7 "This variable is documented")', varAction = 'defvar';
	var codeParam = '(defparameter foo-param 2)', codeParam2 = '(defparameter foo-param 3)', codeDocParam = '(defparameter foo2-param 4 "This parameter is documented")', paramAction = 'defparameter';
	var codeConst = '(defconstant foo-const "toto")', codeConst2 = '(defconstant foo-const "tutu")', codeDocConst = '(defconstant foo2-const "titi" "This constant is documented")', constAction = 'defconstant';
	
	var completePb1 = "(get-actions '(case k ((1 2) 'clause1) (3 'clause2) (nil 'no-keys-so-never-seen) ((nil) 'nilslot) ((:four #\v) 'clause4) ((t) 'tslot) (otherwise 'others)))";

	diag('-- WIP (empty code) --');
	tmp = identifyChunks('');
	if(tmp.length !== 1) {
		skip('1 chunk was expected for the empty code but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===','','Empty code identified as such');
		ok(tmp[0].wip,'Empty code is seen as a work-in-progress');
	}

	diag('-- WIP (1 chunk) --');
	tmp = identifyChunks(preCodeFun);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',preCodeFun,'Code unchanged');
		ok(tmp[0].wip,'Code rightly flagged as WIP');
	}

	diag('-- Complete (1 chunk) --');
	tmp = identifyChunks(codeFun);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',codeFun,'Code unchanged');
		ok(!tmp[0].wip,'Code rightly flagged as Complete');
	}
	
	diag('-- Complete (1 chunk, reader macro) --');
	tmp = identifyChunks(completePb1);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',completePb1,'Code unchanged');
		ok(!tmp[0].wip,'Code rightly flagged as Complete');
	}
	
	diag('-- Several newlines allowed in string --');
	tmp = identifyChunks(codeLargeDocFun);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',codeLargeDocFun,'Code unchanged');
		ok(!tmp[0].wip,'Code rightly flagged as Complete');
	}
	
	diag('-- With #\' + function name --');
	code = "(defun toto (lst) (remove-if #'null lst))";
	tmp = identifyChunks(code);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',code,'Code unchanged');
		ok(!tmp[0].wip,'Code rightly flagged as Complete');
	}
	
	diag('-- With #\' + lambda form --');
	code = "(defun toto (lst) (remove-if #'(lambda (x) (null x)) lst))";
	tmp = identifyChunks(code);
	if(tmp.length !== 1) {
		skip('1 chunk was expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',2);
	} else {
		cmpOK(tmp[0].code,'===',code,'Code unchanged');
		ok(!tmp[0].wip,'Code rightly flagged as Complete');
	}

	diag('-- [C,C,WIP] --');
	code = codeConst;
	code += ' ';
	code += codeVar;
	code += '\n\n\n';
	code += preCodeFun;
	tmp = identifyChunks(code);
	if(tmp.length !== 3) {
		skip('3 chunks were expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',6);
	} else {
		cmpOK(tmp[0].code,'===',codeConst,'First chunk unchanged');
		ok(!tmp[0].wip,'First chunk as Complete');
		cmpOK(tmp[1].code,'===',codeVar,'Second chunk unchanged');
		ok(!tmp[1].wip,'Second chunk as Complete');
		cmpOK(tmp[2].code,'===',preCodeFun,'Third chunk unchanged');
		ok(tmp[2].wip,'Third chunk as WIP');
	}
	

	diag('-- [C,WIP,C] --');
	code = codeConst;
	code += ' ';
	code += preCodeFun;
	code += '\n\n\n';
	code += codeVar;
	tmp = identifyChunks(code);
	if(tmp.length !== 3) {
		skip('3 chunks were expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',6);
	} else {
		cmpOK(tmp[0].code,'===',codeConst,'First chunk unchanged');
		ok(!tmp[0].wip,'First chunk as Complete');
		cmpOK(tmp[1].code,'===',preCodeFun,'Second chunk unchanged');
		ok(tmp[1].wip,'Second chunk as WIP');
		cmpOK(tmp[2].code,'===',codeVar,'Third chunk unchanged');
		ok(!tmp[2].wip,'Third chunk as Complete');
	}


	diag('-- [WIP,WIP,C] --');
	code = preCodeMac;
	code += '\n\n';
	code += preCodeFun;
	code += '\n\n\n';
	code += codeVar;
	tmp = identifyChunks(code);
	if(tmp.length !== 3) {
		skip('3 chunks were expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',6);
	} else {
		cmpOK(tmp[0].code,'===',preCodeMac,'First chunk unchanged');
		ok(tmp[0].wip,'First chunk as WIP');
		cmpOK(tmp[1].code,'===',preCodeFun,'Second chunk unchanged');
		ok(tmp[1].wip,'Second chunk as WIP');
		cmpOK(tmp[2].code,'===',codeVar,'Third chunk unchanged');
		ok(!tmp[2].wip,'Third chunk as Complete');
	}
	
	diag('-- Atoms and unprintable objects --');
	code = codeUnprintableObject;
	code += ' ';
	code += codeAtomFact;
	code += '\n\n';
	code += codeAtomFact;
	code += ' ';
	code += codeUnprintableObject;
	tmp = identifyChunks(code);
	if(tmp.length !== 4) {
		skip('4 chunks were expected but '+tmp.length+' chunks present.\nContent: ['+JSON.stringify(tmp)+']',6);
	} else {
		cmpOK(tmp[0].code, '===', codeUnprintableObject, 'Unprintable object n°1');
		ok(!tmp[0].wip,'Unprintable object considered complete');
		cmpOK(tmp[1].code, '===', codeAtomFact, 'Atom n°1');
		ok(!tmp[1].wip,'Atom considered complete');
		cmpOK(tmp[2].code, '===', codeAtomFact, 'Atom n°2');
		cmpOK(tmp[3].code, '===', codeUnprintableObject, 'Unprintable object n°2');
	}
	
	ddiag('/** -- SAVE & LOAD -- **/');
	
	clearDraftSystem();
	
	cmpOK(loadWipChunk(moduleName, systemName),'===','','No WIP chunk on system creation');
	ok(saveWipChunk(preCodeMac,{'moduleName':moduleName,'systemName':systemName}),'Save one WIP chunk reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeMac,'Save and load one WIP chunk indeed working');
	ok(deleteSavedWipChunk(preCodeMac,moduleName,systemName),'Delete saved WIP chunk reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===','','Delete saved WIP chunk indeed working');
	
	ok(saveWipChunk(preCodeFun+'\n\n'+preCodeMac,{'moduleName':moduleName,'systemName':systemName}),'Save several WIP chunks reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun+'\n\n'+preCodeMac,'Save and load several WIP chunks indeed working');
	
	tmp = '(defun bar-baz (toto';
	ok(saveWipChunk(tmp,{'moduleName':moduleName,'systemName':systemName}),'Save a WIP chunk (to be appended) reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun+'\n\n'+preCodeMac+'\n\n'+tmp,'The WIP chunk was indeed appended');
	
	ok(deleteSavedWipChunk(preCodeMac,moduleName,systemName),'Delete a saved WIP chunk in the middle of the WIP chunks reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun+'\n\n'+tmp,'Delete such WIP chunk indeed working');
	ok(deleteSavedWipChunk(tmp,moduleName,systemName),'Delete a saved WIP chunk at the end of the WIP chunks reported as working');
	cmpOK(loadWipChunk(moduleName,systemName),'===',preCodeFun,'Delete such WIP chunk indeed working');
	
	saveChunks(codeFun,{'moduleName':moduleName,'systemName':systemName});
	cmpOK(loadChunks(moduleName, systemName),'===',codeFun+'\n\n','Save and load complete chunk indeed working');
	saveChunks(codeMac,{'moduleName':moduleName,'systemName':systemName});
	cmpOK(loadChunks(moduleName, systemName),'===',codeFun+'\n\n'+codeMac+'\n\n','Save is cumulative');
	
	ddiag('/** -- FACT OPERATIONS -- **/');
	clearDraftSystem();
	/*code = 'test-id';
	tmp = loadFacts(code,systemName);
	ok($.isArray(tmp) && tmp.length == 0,'loadFacts returns an empty array when there is no associated action to the given id') || diag('Value of loadFacts:\n'+tmp);
	
	tmp = '('+code+' 1 2)';
	cmpOK(saveFact(tmp,{'topAction':code,'systemName':systemName}),'===',null,'saveFact does not work if there is no associated action');
	
	saveChunks('(defun '+code+' (x y) (+ x y))',{'moduleName':moduleName,'systemName':systemName});
	tmp2 = loadFacts(code,systemName);
	ok($.isArray(tmp2) && tmp2.length == 0,'loadFacts returns an empty array when an action has no associated fact') || diag('Value of loadFacts:\n'+tmp2);
	
	isDeeply(saveFact(tmp,{'topAction':code,'systemName':systemName}),[tmp],'saveFact OK');
	isDeeply(loadFacts(code,systemName),[tmp],'loadFacts return the saved fact');
	
	tmp2 = '('+code+' 3 4)';
	isDeeply(saveFact(tmp2,{'topAction':code,'systemName':systemName}),[tmp2],'saveFact n°2 OK');
	isDeeply(loadFacts(code,systemName),[tmp,tmp2],'loadFacts return all the saved facts');
	
	cmpOK(deleteFact(tmp,code,systemName),'===',tmp,'deleteFact OK');
	isDeeply(loadFacts(code,systemName),[tmp2],'loadFacts OK');*/
}