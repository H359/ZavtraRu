from django.contrib.contenttypes.models import ContentType

from zavtra.comments.forms import CommentForm
from zavtra.comments.models import Comment
from zavtra.voting.models import Vote

def get_comments_for(obj, user=None):
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
    context['votes'] = {}
    if user is not None and user.is_authenticated():
	context['votes'] = dict([
	    (v['object_id'], v['vote']) for v in Vote.objects.filter(content_type__id=25, object_id__in = context['comments'], user = user).values('object_id', 'vote')
	])
    return context
