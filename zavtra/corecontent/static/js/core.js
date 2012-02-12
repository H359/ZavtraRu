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

var max = function(arr){
    t = arr[0];
    for (var i = 1, i$l = arr.length; i < i$l; i++) if (arr[i] > t) t = arr[i];
    return t;
}
var min = function(arr){
    t = arr[0];
    for (var i = 1, i$l = arr.length; i < i$l; i++) if (arr[i] < t) t = arr[i];
    return t;
}

jQuery.reduce = function(arr, valueInitial, fnReduce){
    jQuery.each(arr, function(i, value){ valueInitial = fnReduce.apply(value, [valueInitial, i, value]); });
    return valueInitial;
}

$.fn.equalize = function(){
    this.each(function(){
	var row = $(this), spans = row.find('.column');
	if (spans.size() <= 1) return;
	var mh = max(spans.map(function(){ return $(this).height(); }));
	spans.each(function(){ $(this).css({height: mh+'px'}); });
    });
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
	    leftBtn = $('<a class="small btn" href="#">&larr;</a>'),
	    rightBtn = $('<a class="small btn pull-right" href="#">&rarr;</a>');
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
    $('[data-clickable]').css({cursor:'pointer'}).click(function(){window.location = $(this).data('clickable');});
    if (window.comments_bootstrap) window.comments_bootstrap();
    (function(){
	var defParams = {
		'site_charset': "utf-8",
		'ad_format': "direct",
		'font_size': 1,
		'type': "vertical",
		'limit': 4,
		'title_font_size': 3,
		'site_bg_color': "FFFFFF",
		'header_bg_color': "FEEAC7",
		'title_color': "A4322F",
		'url_color': "A4322F",
		'all_color': "A4322F",
		'text_color': "222222",
		'hover_color': "CF3A2D",
		'favicon': true
	    };
	$('.yandex-direct').each(function(k,v){
	    var that = $(this),
		hght = that.parents('.row-fluid').children().height(),
		limit = Math.floor( hght / 100 );
	    console.log(limit);
	    /*
	    window['yandex_context_callbacks'] = window['yandex_context_callbacks'] || [];
	    window['yandex_context_callbacks'].push(function() {
		defParams['limit'] = limit;
		Ya.Direct.insertInto(82666, v, defParams);
	    });
	    */
	});
	if (window['yandex_context_callbacks'] && window['yandex_context_callbacks'].length) {
	    t = document.documentElement.firstChild;
	    s = document.createElement("script");
	    s.type = "text/javascript";
	    s.src = "http://an.yandex.ru/system/context.js";
	    s.setAttribute("async", "true");
	    t.insertBefore(s, t.firstChild);
	}
    })();
    $('.equalized .row-fluid').equalize();
    //$('.carousel').carousel();
    $('a[rel=facebox]').lightBox();
    $('a[rel=popover]').popover();
});