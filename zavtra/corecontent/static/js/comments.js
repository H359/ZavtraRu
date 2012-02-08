!function($){
    var Comments = function(root, settings) {
	this.settings = $.extend({}, $.fn.comments.defaults, settings);
	this.root     = root;
	this.formWrap = $(this.settings.form, this.element);
	this.form     = $('form', this.formWrap);
	this.root.delegate(this.settings.actions, 'click', $.proxy(this.action, this));
	this.form.submit($.proxy(this.submit, this));
	this.enabled = true;
	this.votable = null;
	$('textarea', this.form).markItUp({
	    markupSet: [
		{name: 'Полужирный', key: 'B', openWith: '**', closeWith: '**'},
		{name: 'Курсив', key: 'I', openWith: '_', closeWith: '_'},
		{name: 'Ссылка', key: 'L', openWith: '[', closeWith: ']([![Url:!:http://]!] "[![Title]!]")', placeHolder: 'Cсылка'}
	    ],
	    onShiftEnter: {keepDefault: false, openWith: '\n\n'}
	});
    }

    Comments.prototype = {
	submit: function(e){
	    if (this.enabled){
		this.enabled = false;
		var data = this.form.serialize();
		$('input,select,textarea,button,button', this.form).attr('disabled', true).addClass('disabled').removeClass('error');
		$('span.error', this.form).fadeOut();
		$('div.error', this.formWrap).hide();
		$(this.settings.fieldWrap, this.form).removeClass('error');
		$.post(this.form.attr('action'), data, $.proxy(this.onSubmit, this));
	    }
	    return false;
	},
	resetForm: function(){
	    $('textarea', this.form).val('');
	    $(this.settings.parentField, this.form).val(null);
	},
	onSubmit: function(data){
	    $('input,select,textarea,button,button', this.form).attr('disabled', false).removeClass('disabled');
	    if (data['status']) {
		var p = this.formWrap.parents(this.settings.item);
		this.resetForm();
		if (p.size() == 0) p = $(this.settings.item + ':last-child', this.root);
		if (p.size() == 0) {
		    $(data['html']).appendTo(this.root);
		} else {
		    $(data['html']).insertAfter(p);
		}
		this.formWrap.insertAfter(this.root);
	    } else {
		$('.error', this.formWrap).fadeIn();
		for (var i in data['errors']) {
		    var inp = $('*[name=' + i + ']', this.form).addClass('error'), parent = inp.parent();
		    $('<span class="error help-inline">'+data['errors'][i].join('\n') + '</span>').appendTo(parent);
		    parent.parents(this.settings.fieldWrap).addClass('error');
		}
	    }
	    this.enabled = true;
	},
	hideComment: function(elt, id){
	    $.post('/comments/delete/', {'id': id}, function(data){
		elt.replaceWith($(data['html']));
	    });
	    
	},
	voteComment: function(elt, params){
	    if (this.votable) return;
	    this.votable = elt;
	    $.post('/vote/', {'vote': params[1], 'djct': 25, 'djoi': params[2]}, $.proxy(this.onCommentVoted, this));
	},
	onCommentVoted: function(data){
	    //console.log(data);
	    $('a.comment-rating', this.votable).text(parseInt(data['rating']));
	    if (data['vote'] == 1) {
		$('.vote-minus', this.votable).removeClass('disabled')
		$('.vote-plus', this.votable).addClass('disabled');
	    } else {
		$('.vote-minus', this.votable).addClass('disabled');
		$('.vote-plus', this.votable).removeClass('disabled');
	    }
	    this.votable = null;
	},
	action: function(e){
	    var target = $(e.target);
	    if (e.target.nodeName != 'A') target = target.parents('a');
	    var ops    = target.attr('href').split('#')[1].split(':'),
		cmnt   = target.parents(this.settings.item),
		prnt   = this.settings.parentExtractor(cmnt);
	    switch (ops[0]) {
		case 'reply':
		    this.formWrap.appendTo(cmnt);
		    $(this.settings.parentField, this.form).val(prnt);
		    break;
		case 'expand':
		    var more  = $('span.hide', cmnt),
			text  = more.html().trim(),
			otext = $('.text', cmnt);
		    more.remove();
		    var ohtml = otext.html().trim();
		    otext.html( ohtml.substring(0, ohtml.length-1) + text );
		    target.remove();
		    break;
		case 'hide':
		    this.hideComment(cmnt, prnt);
		    break;
		case 'vote':
		    this.voteComment(cmnt, ops);
		    break;
	    }
	    return false;
	}
    }

    $.fn.comments = function(settings){ new Comments(this, settings); }

    $.fn.comments.defaults = {
	 form: 'div.comment-form'
	,item: '#comments > li'
	,fieldWrap: 'div.clearfix'
	,parentExtractor: function(elt) { return $(elt).attr('id').split('-')[1]; }
	,parentField: '#id_parent'
	,actions: 'div.comment div.btn-group a.btn'
    }
}( window.jQuery || window.ender );