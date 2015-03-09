$(document).ready(function() {
	var editor = $('#editor'), workInProgress = false, prevValue;
	
	var save = function() {
		var toSave = editor.val();
		if(workInProgress && toSave !== '') {
			saveArticleContent(toSave, function() {
					workInProgress = false;
					prevValue = editor.val();
					
					editor.animate({
						backgroundColor: "#DDD"
					},300).animate({
						backgroundColor: "#FFF"
					},150);
				});
		}
	};
	
	var load = function() {
		loadArticleContent(function(data) {
				editor.val(data);
				prevValue = data;
			});
	};
	
	// Keyboard shortcut to save
	$(document).on('keydown', function(e) {
		if(e.ctrlKey && e.which === 81) { // C - Q
			if(workInProgress) {
				save();
			} else {
				showEditorList();
			}
		}
	});
	
	// Detect changes
	var detectChanges = function() {
		if(prevValue !== editor.val()) {
			workInProgress = true;
		}
	};
	
	editor.focus(function() {
		$(document).bind('keydown', detectChanges);
	});
	
	editor.blur(function() {
		$(document).unbind('keydown', detectChanges);
	});
	
	// Save on window close
	window.onunload = save;
	
	// Actually insert tab when tab is pressed
	editor.keydown(function(e) {
		var keyCode = e.keyCode || e.which, start, end;
		
		if (keyCode === 9) {
			e.preventDefault();
			start = editor.get(0).selectionStart;
			end = editor.get(0).selectionEnd;
			
			// set textarea value to: text before caret + tab + text after caret
			editor.val(editor.val().substring(0, start)
				+ "\t"
				+ editor.val().substring(end));

			// put caret at right position again
			editor.get(0).selectionStart =	editor.get(0).selectionEnd = start + 1;
		}
	});
	
	load();
});