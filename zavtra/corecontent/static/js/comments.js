$(document).ajaxSend(function(event, xhr, settings) {
    function getCookie(name) {
        var cookieValue = null;
        if (document.cookie && document.cookie != '') {
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
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
    if (!safeMethod(settings.type) && sameOrigin(settings.url)) { xhr.setRequestHeader("X-CSRFToken", getCookie('csrftoken')) };
});

var ThouMakethCommentsShine = function(num, box) {
    var box = $(box), form = $('form', box), sbmt = $('button[type=submit]', box);

    var state = true;
    var toggle = function(){
        sbmt.attr('disabled', state);
        $('textarea', box).attr('disabled', state).val('');
        state = !state;
    }
    
    var handleFormSubmission = function(data){
        if (!data['status']) {
            // make errors shine
        } else {
    	    var root = $('#comment-' + $('input[name=parent]', form).val());
    	    if (root.size() == 0) { 
    		$('dl', box).append(data['html']);
    	    } else {
    		$(data['html']).insertAfter( $('#comment-' + $('input[name=parent]', form).val()) )
    	    }
            toggle();
            form.insertAfter($('dl',box));
        }
    }
    
    var submitForm = function(){
        $.post(form.attr('action'), form.serialize(), handleFormSubmission);
        toggle();
        return false;
    }

    var moveForm = function(){
	var self = $(this);
	var li = self.parents('dt');
	$('input[name=parent]', form).val(li.attr('id').split('-')[1]);
	li.append(form);
	return false;
    }

    var deleteComment = function(){
	var self = $(this);
	var li = self.parents('dt');
	var root = li.attr('id').split('-')[1];
	$.post('/comments/delete/', {id: root}, function(){
	    li.html('скрыто');
	});
	return false;
    }

    $('.buttons a.reply', box).on('click', moveForm);
    $('.buttons a.delete', box).on('click', deleteComment);
    $('a.supress-long-toggle', box).on('click', function(){
	var self = $(this);
	$('span.supress-long', self.parent()).show();
	self.prev('br').remove();
	self.remove();
	return false;
    });

    form.submit(submitForm);
}
$(document).ready(function(){
  $('div.comments').each(ThouMakethCommentsShine);
});
