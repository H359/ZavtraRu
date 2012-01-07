var ajaxFields = function(opts){
    $ = django.jQuery;
    var element = $('#' + opts['name']),
	rt = null,
	lastQuery = null,
	filter = $('.selector-filter input', element);
    if (opts['multiple']) {
	form = element.parents('form'),
	addButton = $('.selector-add', element),
	remButton = $('.selector-remove', element),
	leftside = $('.selector-available select'),
	rightside = $('.selector-chosen select'),
	moveOptions = function(src,dst){ return $('option:selected', src).remove().appendTo(dst); },
	l2r = function(){ return !moveOptions(leftside, rightside); },
	r2l = function(){ return !moveOptions(rightside, leftside); };
	
	addButton.click(l2r);
	remButton.click(r2l);
	leftside.live('dblclick', 'option', l2r);
	rightside.live('dblclick', 'option', r2l);

	form.submit(function(){
	    $('option', rightside).each(function(){ $(this).attr('selected', true); });
	    return true;
	});

    } else {
	var leftside = $('select', element);
	leftside.attr('size', '10');
    }

    var renderDelegate = function(d){
        $('option', leftside).remove();
        $.each(d, function(k,v){ console.log(v); $('<option value="'+v[0]+'">'+v[1]+'</option>').appendTo(leftside); });
    }

    var simplify = function(obj){
	var res = [];
	for (var k in obj) { res.push(k); res.push(obj[k]); }
	return res.join('|');
    }

    var issueRequest = function(){
	var val = filter.val().trim();
	if (val.length > 3 && lastQuery != val) {
	    $.ajax({
		url: opts['endpoint'],
		data: {
		    'app': opts['app'],
		    'guard': simplify(opts['guard']),
		    'model': opts['model'],
		    'fields': opts['fields'].join(','),
		    'q': val
		},
		success: function(d){
		    renderDelegate(d);
		    lastQuery = val;
		    rt = null;
		},
		type: 'GET'
	    });
	}
    }

    filter.keydown(function(e){
	if (rt) clearTimeout(rt);
	rt = setTimeout(issueRequest, 1000);
    });
}