$(document).ready(function() {
	$('body').append('<div id="dialog"></div>');
	$('#dialog').dialog({'autoOpen': false, 'modal': true, 'title': 'Editors'});
});

function showEditorList() {
	$.ajax('/list-editors',
		{'async': true,
		'type': 'GET',
		'success': function(data) {
			var html = '', arr = JSON.parse(data), currentSystem;
			
			currentSystem = window.location.pathname.slice(1);
			currentSystem = currentSystem.slice(0, currentSystem.indexOf('/'));
			
			for(var i=0; i<arr.length; i++) {
				if(arr[i] !== currentSystem) {
					html += '<li><a href="/' + arr[i] + '/editor">'+arr[i]+'</li>';
				}
			}
			if(html.length > 0) {
				$('#dialog').html('<ul>'+html+'</ul>');
				$('#dialog').dialog("open");
			}
		}});
}