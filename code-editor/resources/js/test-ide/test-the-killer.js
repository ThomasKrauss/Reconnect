$(document).ready(function() {
	// The ability to clear the draft system is mandatory for all
	// the partial tests which, unlike IDE tests, do not seek to manage
	// systems.
	// As such, before any script files start its tests, it calls
	// clearDraftSystem()
	// So it must work!
	if(clearDraftSystem()) {
		var tsBrowser =	new Test.Harness.Browser(
			'tools/test-simple/Builder.js',
			'tools/test-simple/More.js'
		);
		
		tsBrowser.args.verbose = true;
		
		tsBrowser.runTests(
			'test-ide/test-functions.js',
			'test-ide/test-editable-chunks.js',
			'test-ide/test-editing-area.js'
		);
	} else {
		console.log('Test the killer: Clear draft system failed');
	}
	
	
	$('body').append('<hr /><p>Interactive tests</p>');
	
	$('body').append('<p>Please select the editor, *in* the action perimeter, and type other stuff in it, then click done.</p>');
	$('body').append('<p>We want to validate the editor does not fire onChunkCompletion several times, only once, on focus out</p>');
	$('body').append('<div id="ecInteractive"></div>');
	$('body').append('<input id="ecSubmit" type="button" value="Done" />');
	
	// Event checking
	var ec, evtChunkCompletion;
	var resetEvtFlags = function() {
		evtChunkCompletion = [];
	};
	var flagChunkCompletion = function(completedCode) {
		evtChunkCompletion.push(completedCode);
	};
	var onChunkCompletionFired = function() {
		return !(evtChunkCompletion.length == 0);
	};
	
	// Components installation
	var installEC = function(codeChunk) {
		var realCodeChunk = codeChunk === undefined ? '' : codeChunk;

		$('#ecInteractive').empty();
		resetEvtFlags();
		ec = makeEditableChunk(document.getElementById('ecInteractive'),
				{onChunkCompletion: flagChunkCompletion, initChunk: {'code': realCodeChunk}});
	};
	
	$('#ecSubmit').click(function() {
		var text;
		
		if(onChunkCompletionFired()) {
			if(evtChunkCompletion.length != 1) {
				text = 'KO! onChunkCompletionFired was wrongly fired '+evtChunkCompletion.length+' times!';
				text += '\nDetails:\n'+JSON.stringify(evtChunkCompletion);
			} else {
				text = 'OK! onChunkCompletionFired was fired exactly one time!';
				text += '\nThe completed chunk it gave is '+evtChunkCompletion[0];
			}
		} else {
			text = 'KO! onChunkCompletionFired was not fired!';
		}
		
		$('body').append('<p>'+text+'</p>');
	});
	
	// Init
	installEC('(foo-fun 1 2)');
	resetEvtFlags();
});