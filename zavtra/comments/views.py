#from django.contrib.contenttypes.models import ContentType
from django.db import models
from django.template.loader import render_to_string
from django.contrib.auth.decorators import user_passes_test, login_required

from annoying.decorators import ajax_request

#from corecontent.models import ContentItem
from models import Comment
from forms import CommentForm

@ajax_request
@login_required
def add_comment(request):
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
    	    """
    	    if parent is None:
    		comment = Comment.add_root(**data)
    	    else:
    		comment = Comment.objects.get(id=parent).add_child(**data)
    	    """
    	    comment = Comment.objects.create(**data)
            res['status'] = True
            res['html'] = render_to_string('comments/item.html', {'request': request, 'comment': comment})
        else:
            res['errors'] = dict( (k, map(unicode, v)) for (k,v) in form.errors.iteritems() )
    return res

@ajax_request
@user_passes_test(lambda u: u.has_perm('comments.moderate'))
def delete_comment(request):
    comment = Comment.objects.get(pk=request.POST.get('id'))
    comment.enabled = not comment.enabled
    comment.save()
    permsFake = {'comments': {'moderate': True}}
    return {'enabled': comment.enabled, 'html': render_to_string('comments/item.html', {'request': request, 'comment': comment, 'perms': permsFake})}