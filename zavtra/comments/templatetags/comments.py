from django import template

from django.contrib.contenttypes.models import ContentType
from zavtra.comments.forms import CommentForm
from zavtra.comments.models import Comment

register = template.Library()

@register.inclusion_tag('comments.list.html', takes_context=True)
def get_comments_for(context, obj):
    ctype = ContentType.objects.get_for_model(obj.__class__)
    initial_data = {
        'parent': None,
        'content_type': ctype,
        'object_id': obj.id
    }
    context['comments'] = Comment.objects.filter(content_type=ctype, object_id=obj.id).select_related()
    context['form'] = CommentForm(initial=initial_data)
    return context
