#from django.contrib.contenttypes.models import ContentType
from django.db import models

from annoying.decorators import ajax_request

#from corecontent.models import ContentItem
#from models import Comment
from forms import CommentForm

@ajax_request
def add_comment(request):
    """
    comment = Comment(
        parent=None,
        content_object=ContentItem.objects.get(id=request.POST.get('object_id')),
        author=request.user,
        comment=request.POST.get('text')
    )
    comment.save()
    return {'comment': comment}
    """
    res = {'status': False}
    if request.method == 'POST':
        form = CommentForm(request.POST)
        if form.is_valid():
            res['status'] = True
            instance = form.save(commit=False)
            instance.author = request.user
            instance.enabled = True
            instance.save()
            res['comment'] = {}
            for field in instance._meta.fields:
                if isinstance(field, models.ForeignKey) or isinstance(field, models.ManyToManyField):
                    continue
                res['comment'][field.name] = getattr(instance, field.name)
            res['comment']['parent'] = instance.parent_id
            res['comment']['created_at'] = res['comment']['created_at'].isoformat()
        else:
            res['errors'] = dict( (k, map(unicode, v)) for (k,v) in form.errors.iteritems() )
    return res
