var currentWidth;
var max = function(l) {
	var res = l[0], i$l = l.length;
	for (var i = 0; i < i$l; i++) if (res < l[i]) res = l[i];
	return res;
}

var alignRow = function(num, row) {
	var children = $('.article-content', row);
	if (children.length > 1) children.height(max(children.map(function(i,v){return $(v).height();})));
}

var alignRows = function() {
	$('.rows-aligned .span12 .row').each(alignRow);
}

function checkSizes() {
		if ($(window).width() < 768) {
			if ((currentWidth >= 768 || currentWidth === undefined) && ($('#main_gray_line').css('display') == 'none')) { /* the last condition is required to cope with weird firefox behavior */
				$('#newspaper_block').insertAfter('#wod_block');
				$('#navigation_block').insertBefore('#events_block');
				$('#subjects_block').insertAfter('#navigation_block');
			}
		} else if ($(window).width() >= 768 && currentWidth < 768) {
			$('#navigation_holder').append($('#navigation_block'));
			$('#newspaper_holder').append($('#newspaper_block'));
			$('#subjects_holder').append($('#subjects_block'));
		}
		currentWidth = $(window).width();
}

function switchMainTabs(which) {

	$('.main_tab.active').toggleClass('active');
			
	switch ($(which).attr('id')) {
		case 'main_blogs_new_switch':
			$('#main_blogs_new_switch').toggleClass('active');
			$('#main_blogs_new').css('display', 'block');
			$('#main_blogs_favorite').css('display', 'none');
			$('#main_blogs_polemics').css('display', 'none');
		break

		case 'main_blogs_polemics_switch':
			$('#main_blogs_polemics_switch').toggleClass('active');
			$('#main_blogs_new').css('display', 'none');
			$('#main_blogs_favorite').css('display', 'none');
			$('#main_blogs_polemics').css('display', 'block');
		break

		case 'main_blogs_favorite_switch':
			$('#main_blogs_favorite_switch').toggleClass('active');
			$('#main_blogs_new').css('display', 'none');
			$('#main_blogs_favorite').css('display', 'block');
			$('#main_blogs_polemics').css('display', 'none');
		break
	}
}

var collectPictures = function(){
	var page = $('#page .article-page-content'),
			thmb = $('#page .article-page-illustration img'),
			imgs = page.find('img');
	var carousel = $("<div id='article-images-slider' class='carousel slide'></div>");
	var inner = $("<div class='carousel-inner'></div>");
	if (imgs.length > 0) {
	    page.prepend(carousel);
	    carousel.append(inner);
	    inner.css({height: '400px'});
	    var appender = function(k,v){
		var row = $("<div class='item'></div>");
		inner.append(row);
		row.append($(v));
		if (k == 0) row.addClass('active');
	    };
	    imgs.each(appender);
	    thmb.hide();
	    carousel.append(
		$("<a href='#article-images-slider' class='carousel-control left' data-slide='prev'>&lsaquo;</a>")
	    );
	    carousel.append(
		$("<a href='#article-images-slider' class='carousel-control right' data-slide='next'>&rsaquo;</a>")
	    );
	    carousel.carousel();
	}
}

$(document).ready(function(){
	checkSizes();
	$('.section_header_button_datepicker').on('click', function(){
		$(this).toggleClass('selected');
		$('.datepicker_container').toggle();
		return false;
	});
	$('.main_tab').on('click', function(){switchMainTabs(this)});
	$('.carousel-fast').carousel({interval: 8000});
	$('.carousel-slow').carousel({interval: 14000});
	$('#main_header_login_link').on('click', function(){
		$('#login-modal').modal();
		return false;
	});
	$('#main_video_selected').find("[data-video-source]").on('click', function(){
		var video_source = $(this).data('video-source').split(':'), src;
		if (video_source[0] == 'youtube') {
			src = "http://www.youtube.com/embed/" + video_source[1];
		} else {
			src = "http://dentv.ru/embed" + video_source[1] + '0/';
		}
		var video = $('<div class="modal-header"><button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button></div><div class="modal-body" style="text-align:center"><iframe src="' + src + '" width="640" height="360" frameborder="0" allowfullscreen></iframe></div>');
		$('#video-modal').empty().append(video).modal();
		return false;
	});
	if (/^\/content\/view/.test(window.location.pathname)) collectPictures();
});

$(window).on('resize', checkSizes);
$(window).on('load', alignRows)

//$.fn.modal.defaults.maxHeight = function(){ return $(window).height() - 165; }