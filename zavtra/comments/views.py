#from django.contrib.contenttypes.models import ContentType
from django.db import models
from django.template.loader import render_to_string

from annoying.decorators import ajax_request

#from corecontent.models import ContentItem
#from models import Comment
from forms import CommentForm

@ajax_request
def add_comment(request):
    res = {'status': False}
    if request.method == 'POST':
        form = CommentForm(request.POST)
        if form.is_valid():
            res['status'] = True
            instance = form.save(commit=False)
            instance.author = request.user
            instance.enabled = True
            instance.save()
            # TODO: compile template
            res['html'] = render_to_string('comments/item.html', {'comment': instance})
        else:
            res['errors'] = dict( (k, map(unicode, v)) for (k,v) in form.errors.iteritems() )
    return res
