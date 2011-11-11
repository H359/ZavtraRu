if (django) $ = jQuery = django.jQuery;

$(document).ready(function(){
    $('.wymeditor').wymeditor({
        'updateSelector': 'input:submit',
        'updateEvent': 'click',
        'skin': 'default',
        'lang': 'ru'
    });
});
