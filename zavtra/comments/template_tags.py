from django.contrib.contenttypes.models import ContentType
from zavtra.comments.forms import CommentForm
from zavtra.comments.models import Comment

def get_comments_for(obj):
    context = {}
    ctype = ContentType.objects.get_for_model(obj.__class__)
    initial_data = {
        'parent': None,
        'content_type': ctype,
        'object_id': obj.id
    }
    context['comments'] = Comment.objects.filter(content_type=ctype, object_id=obj.id).select_related()
    context['commentable'] = obj
    context['form'] = CommentForm(initial=initial_data)
    return context
