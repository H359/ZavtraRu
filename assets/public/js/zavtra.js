var currentWidth;
var max = function(l) {
	var res = l[0], i$l = l.length;
	for (var i = 0; i < i$l; i++) if (res < l[i]) res = l[i];
	return res;
}

var alignRow = function(num, row) {
	var children = $('.article-content', row);
	if (children.length > 1) children.height(max(children.map(function(i,v){ return $(v).height(); })));
}

var alignRows = function() {
	$('.rows-aligned .row').each(alignRow);
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

$(document).ready(function(){
	checkSizes();
	$('.main_tab').on('click', function(){switchMainTabs(this)});
	$('#social').css({left: $('#page').css('marginLeft')});
});

$(window).on('resize', checkSizes);
$(window).on('load', alignRows)