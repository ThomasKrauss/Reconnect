/** @jsx React.DOM */

GlobalEvents = makeEventManager();
EndPoints = makeEndPoints();

var makeView = function(domElement, endPointName) {
	var refreshDebuggedUsages = function(data) {
		if(data && data.value && data.value.html) {
			$('#debug').html(data.value.html).show();
			$('#doc').hide();
		} else {
			$('#debug').empty().hide();
			$('#doc').show();
		}
	};
	
	var refreshDocumentation = function(data) {
		if(data && data.value && data.value.html) {
			$('#doc').html(data.value.html);
		} else {
			$('#doc').empty();
		}
	};
	
	var reset = function() {
		htmlFragment = {};
		EndPoints.on('debug/debugged-usages', refreshDebuggedUsages);
		EndPoints.on('doc/module-doc', refreshDocumentation);
	};
	reset();
};

$(document).ready(function() {
	$('#layout').append('<div id="debug" class="debug"></div>');
	$('#layout').append('<div id="doc" class="doc"></div>');
	makeView();
	
	// makeHtmlFragment(document.getElementById('debug'), 'debug/debugged-usages');
	// makeHtmlFragment(document.getElementById('debug'), 'doc/module-doc');
});