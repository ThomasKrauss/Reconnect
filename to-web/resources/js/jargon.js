$(function(){
	var cache = {};
	
	$(window).bind( 'hashchange', function(e) {
		var fullUrl, urlHash;

		// Must not take the '#'!
		urlHash = window.location.hash.substring(1);

		// Recompose the url: take the base path only excluding the filename and hash
		// Then append the hash
		fullUrl = window.location.protocol + '//' + window.location.host + window.location.pathname;
		fullUrl = fullUrl.slice(0, fullUrl.lastIndexOf('/')+1) + urlHash;
		
		// Refresh the current style (only if urlHash is not empty)
		$('a.current').removeClass('current');
		urlHash && $('a[href$="' + urlHash + '"]').addClass('current');

		if ( cache[urlHash] === undefined ) {
			if(urlHash !== '') {
				$.ajax(fullUrl, {'async': false, 'success': function(data) {
					cache[urlHash] = data;
				}});
			} else {
				cache[urlHash] = $('.jargon .article').html();
			}
		}
		
		$('.jargon .article').html( cache[urlHash] );
	});

	// Since the event is only triggered when the hash changes, we need to trigger
	// the event now, to handle the hash the page may have loaded with.
	$(window).trigger( 'hashchange' );
	
	// Listen to hyperlink clicks
	$('.menu a').click(function(e) {
		e.preventDefault();
		e.stopPropagation();
		
		var pieces = $(this).attr('href').split('/');
		window.location.hash = pieces[pieces.length-2] + '/' + pieces[pieces.length-1];
	});
});