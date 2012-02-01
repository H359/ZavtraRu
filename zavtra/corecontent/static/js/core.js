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

/*
$.fn.makethScroll = function(){
    var that = $(this),
	mouseCapture = false,
	content = $('li', that),
	content$width = $.reduce(content, 0, function(acc, num, item){ return acc + $(item).outerWidth(); }),
	wrapper = that.parent('div').css({overflowX:'hidden'}),
	wrapper$width = wrapper.width(),
	scrollbar = $('<div class="scroller"></div>'),
	handle = $('<div class="handle"></div>'),
	leftArr = $('<input type="button" class="btn arrow left" value="&larr;" />'),
	rightArr = $('<input type="button" class="btn arrow right" value="&rarr;" />');
    that.css({
	position: 'relative',
	width: content$width + 'px'
    });
    scrollbar.appendTo(wrapper);
    leftArr.appendTo(wrapper);
    rightArr.appendTo(wrapper);
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
    });
    rightArr.mousedown(function(){
    })
    handle.mousedown(function(e){ mouseCapture = true; });
}
*/
$(function(){
    //$('ul.scrollable').makethScroll();
    (function(){
	var scrollContent = $('ul.scrollable'),
	    scrollPane = scrollContent.parent('div'),
	    scrollContent$width = $.reduce($('li', scrollContent), 0, function(acc,num,item){ return acc + $(item).outerWidth(); }),
	    scrollPane$width = scrollPane.width(),
	    diff = (scrollPane$width - scrollContent$width),
	    proportion = diff / scrollContent$width,
	    handle$width = scrollPane$width - (proportion + scrollPane$width),
	    animating = false,
	    scroller = null;
	scrollContent.width(scrollContent$width);
	var slideFunc = function(e,ui){
	    if (animating) scrollContent.stop();
	    animating = true;
	    scrollContent.animate({left: Math.round(diff*ui.value/100) + 'px'}, {
		complete: function() { animating = false; }
	    });
	};
	scroller = $('#slider').slider({change: slideFunc, animate: true});
	scroller.find('ui-slider-handle').css({width: handle$width + 'px', marginLeft: (-handle$width/2) + 'px'});
	var resetValues = function(){
		scrollPane$width = scrollPane.width();
		diff = (scrollPane$width - scrollContent$width);
		proportion = diff / scrollContent$width;
		handle$width = scrollPane$width - (proportion + scrollPane$width);
	    },
	    wrapper = scroller.parent('div'),
	    leftBtn = $('<a class="btn" href="#">&larr;</a>'),
	    rightBtn = $('<a class="btn pull-right" href="#">&rarr;</a>');
	leftBtn.appendTo(wrapper);
	rightBtn.appendTo(wrapper);
	var slideTo = function(dir){
	    return function(){
		var val = scroller.slider('value'),
		    newval = Math.round(val+dir);
		if (newval >= 0 && newval <= 100) {
		    scroller.slider('value', newval);
		    slideFunc(null, {'value': newval});
		}
		return false;
	    };
	};
	leftBtn.click(slideTo(-handle$width*10));
	rightBtn.click(slideTo(handle$width*10));
	$(window).resize(resetValues);
    })();
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