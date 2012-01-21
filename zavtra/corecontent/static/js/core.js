$(document).ajaxSend(function(event, xhr, settings) {
    function sameOrigin(url) {
        // url could be relative or scheme relative or absolute
        var host = document.location.host; // host + port
        var protocol = document.location.protocol;
        var sr_origin = '//' + host;
        var origin = protocol + sr_origin;
        // Allow absolute or scheme relative URLs to same origin
        return (url == origin || url.slice(0, origin.length + 1) == origin + '/') ||
            (url == sr_origin || url.slice(0, sr_origin.length + 1) == sr_origin + '/') ||
            // or any other URL that isn't scheme relative or absolute i.e relative.
            !(/^(\/\/|http:|https:).*/.test(url));
    }
    function safeMethod(method) {
        return (/^(GET|HEAD|OPTIONS|TRACE)$/.test(method));
    }
    if (!safeMethod(settings.type) && sameOrigin(settings.url)) { xhr.setRequestHeader("X-CSRFToken", $.cookie('csrftoken')) };
});

var rs = null;
var pfp = function(){
        var rsw = rs.width() - 90;
	$('.item', rs).width(rsw);
	return rsw;
}

$(function(){
    (function(){
	$('.sidebar .collapse').click(function(){
	    var collapsed = $('body').toggleClass('collapsed').hasClass('collapsed');
	    $.cookie('news_collapsed', collapsed, {expires:365});
	    $(this).html( $(this).html() == '&gt;&gt;&gt;' ? '&lt;&lt;&lt;' : '&gt;&gt;&gt;' );
	});
	rs = $('#red_stripe');
	var rsw = pfp();
	$('.scrollable', rs).width(rsw).show()
	//$('.scrollable', 
	$('.scrollable').scrollable({circular:true});
    })();
    if ($.browser.msie){
	if ($.browser.version.substr(0,1)<=7) {
	    $('.topbar, body > .container-fluid').hide();
	    CFInstall.check({mode:'overlay',destination:'http://zavtra.ru'});
	}
    }
});

$(window).resize(function(){ var rsw = pfp(); $('.scrollable', rs).width(rsw); });