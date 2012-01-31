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

jQuery.reduce = function(arr, valueInitial, fnReduce){
    jQuery.each(arr, function(i, value){ valueInitial = fnReduce.apply(value, [valueInitial, i, value]); });
    return valueInitial;
}

$.fn.makethScroll = function(){
    var that = $(this),
	mouseCapture = false,
	content = $('li', that),
	content$width = $.reduce(content, 0, function(acc, num, item){ return acc + $(item).outerWidth(); }),
	wrapper = that.parent('div').css({overflowX:'hidden'}),
	wrapper$width = wrapper.width(),
	scrollbar = $('<div class="scroller"></div>'),
	handle = $('<div class="handle"></div>');
    that.css({
	position: 'relative',
	width: content$width + 'px'
    });
    scrollbar.appendTo(wrapper);
    handle.appendTo(scrollbar);
    var handle$width = handle.width(),
	k1 = (wrapper$width - handle$width) / wrapper$width,
	k2 = (content$width - wrapper$width) / (wrapper$width - handle$width),
	changePos = function(x){
	    that.css({left: -Math.floor(k2*x) + 'px'});
	    handle.css({left: Math.floor(k1*x) + 'px'});
	},
	getOffsetX = function(e){
	    if (e.originalEvent && e.offsetX === undefined) return e.originalEvent.layerX;
	    return e.offsetX;
	};
    $(window).resize(function(){
	wrapper$width = wrapper.width();
	handle$width = handle.width();
	k1 = (wrapper$width - handle$width) / wrapper$width;
	k2 = (content$width - wrapper$width) / (wrapper$width - handle$width);
    });
    scrollbar.click(function(e){
	if (!mouseCapture && e.target == scrollbar[0]) changePos(getOffsetX(e));
	return false;
    }).mousemove(function(e){
	if (mouseCapture && e.target == scrollbar[0]) changePos(getOffsetX(e));
    }).mouseup(function(e){ 
	if (mouseCapture && e.target == scrollbar[0]) changePos(getOffsetX(e));
	mouseCapture = false;
    })
    handle.mousedown(function(e){ mouseCapture = true; });
}

$(function(){
    $('ul.scrollable').makethScroll();
    $('[data-clickable]').css({cursor:'pointer'}).click(function(){window.location =$(this).data('clickable');});
    if (window.comments_bootstrap) window.comments_bootstrap();
    /*
    var body=$('body')[0];
    $('.bnrok').each(function(){var self=$(this);
	var bscr = document.createElement('script'), bid = self.attr('id').split('_')[2];
	bscr.type='text/javascript';
	bscr.charset='windows-1251';
	bscr.src='http://www.directadvert.ru/show.cgi?adp='+bid+'&div='+self.attr('id')+'&nnn='+bid+'&t='+Math.random();
	//bscr.src='http://www.directadvert.ru/show.cgi?adp='+bid+'&div='+self.attr('id');
	body.appendChild(bscr);
    });
    */
});