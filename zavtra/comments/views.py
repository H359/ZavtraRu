#from django.contrib.contenttypes.models import ContentType
from django.shortcuts import Http404, redirect
from django.db import models
from django.template.loader import render_to_string
from django.contrib.auth.decorators import user_passes_test, login_required
from utils import templateLookup

from annoying.decorators import ajax_request

#from corecontent.models import ContentItem
from models import Comment
from forms import CommentForm

@ajax_request
@login_required
def add_comment(request):
    if not request.user.is_active:
	raise Http404
    res = {'status': False}
    if request.method == 'POST':
        form = CommentForm(request.POST)
        if form.is_valid():
    	    data = form.cleaned_data
    	    parent = data.get('parent')
    	    del data['parent']
    	    data['parent_id'] = parent
    	    data['author'] = request.user
    	    data['ip'] = request.META.get('REMOTE_ADDR')
    	    comment = Comment.objects.create(**data)
            res['status'] = True
            res['html'] = templateLookup.get_template('base.html').get_def('comment').render_unicode(item=comment, request=request)
        else:
            res['errors'] = dict( (k, map(unicode, v)) for (k,v) in form.errors.iteritems() )
    if not request.is_ajax():
	return redirect(request.POST.get('next'))
    return res

@ajax_request
@user_passes_test(lambda u: u.has_perm('comments.moderate'))
def delete_comment(request):
    res = {}
    comment = Comment.objects.get(pk=request.POST.get('id'))
    # bypass email notification %)
    comment.enabled = not comment.enabled
    Comment.objects.filter(pk=comment.pk).update(enabled=comment.enabled)
    #permsFake = {'comments': {'moderate': True}}
    res['html'] = templateLookup.get_template('base.html').get_def('comment').render_unicode(item=comment, request=request, vote=0)
    res['enabled'] = comment.enabled
    return res
    #return {'enabled': comment.enabled, 'html': render_to_string('comments/item.html', {'request': request, 'comment': comment, 'perms': permsFake})}