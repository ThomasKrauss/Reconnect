var makeTextFlowEditor = function(domElement) {
	var editor;
	var currentFilename, clean, prevValue;
	
	var save = function() {
		if(!clean && currentFilename !== null) {
			saveArticleContent(currentFilename, domElement.val(),
				function(json) {
					clean = true;
					prevValue = domElement.val();
					
					domElement.animate({
						backgroundColor: "#DDD"
					},300).animate({
						backgroundColor: "#FFF"
					},150);
				});
		}
	};
	
	var load = function(filename) {
		loadArticleContent(filename, function(json) {
			currentFilename = filename;
			domElement.val(json);
			prevValue = json;
		});
	};
	
	var isClean = function() {
		return clean;
	};
	
	var detectChanges = function() {
		if(prevValue !== domElement.val()) {
			clean = false;
		}
	};
	
	var reset = function() {
		editor = {};
		currentFilename = null;
		clean = true;
		
		// Detect changes
		domElement.focus(function() {
			$(document).bind('keydown', detectChanges);
		});
		
		domElement.blur(function() {
			$(document).unbind('keydown', detectChanges);
		});
		
		// Actually insert tab when tab is pressed
		domElement.keydown(function(e) {
			var keyCode = e.keyCode || e.which, start, end;
			
			if (keyCode === 9) {
				e.preventDefault();
				start = domElement.get(0).selectionStart;
				end = domElement.get(0).selectionEnd;
				
				// set textarea value to: text before caret + tab + text after caret
				domElement.val(domElement.val().substring(0, start)
					+ "\t"
					+ domElement.val().substring(end));

				// put caret at right position again
				domElement.get(0).selectionStart =	domElement.get(0).selectionEnd = start + 1;
			}
		});
		
		// Nothing loaded at first (prevent cache artifacts)
		domElement.val('');
		
		editor.save = save;
		editor.load = load;
		editor.isClean = isClean;
	};
	reset();
	return editor;
};

$(document).ready(function() {
	var firstEditor = makeTextFlowEditor($('#firstEditor')), secondEditor = makeTextFlowEditor($('#secondEditor'));
	
	// Keyboard shortcut to save
	$(document).on('keydown', function(e) {
		if(e.ctrlKey && e.which === 81) { // C - Q
			if(firstEditor.isClean() && secondEditor.isClean()) {
				showEditorList();
			} else {
				firstEditor.save();
				secondEditor.save();
			}
		}
	});
	
	listMainArticleNames(function(json) {
		var i;
		
		// Fill in the menu
		for(i=0; i<json.length; i++) {
			var name = json[i].slice(1 + json[i].lastIndexOf('-'),json[i].lastIndexOf('.'));
			$("#menu").append('<li><a href="'+json[i]+'">'+name+'</a></li>');
		}
		
		// Menu hover clues
		$("#menu li").mouseover(function() {
			$(this).addClass('boxLink');
		});
		$("#menu li").mouseleave(function() {
			$(this).removeClass('boxLink');
		});
		
		// On menu click, load the article but save if previous was modified!
		$('#menu li').click(function(e) {
			var filename = $(this).children('a:first').attr('href'), order;
			
			e.preventDefault();
			e.stopPropagation();
			
			$('#menu li').removeClass('current');
			$(this).addClass('current');
			
			firstEditor.save();
			firstEditor.load(filename);
			
			order = filename.slice(1 + filename.indexOf('/'), filename.indexOf('-'));
			if(order) {
				getFrenchTranslationArticleName(order, secondEditor.load);
			}
		});
	});
	
	// Save on window close
	window.onunload = function() {
		firstEditor.save();
		secondEditor.save();
	};
});