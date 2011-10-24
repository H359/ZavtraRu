from django import template

from django.contrib.contenttypes.models import ContentType
from zavtra.comments.forms import CommentForm

register = template.Library()

@register.inclusion_tag('comment_form.html')
def comment_form_for(obj):
    initial_data = {
        'parent': None,
        'content_type': ContentType.objects.get_for_model(obj.__class__),
        'object_id': obj.id
    }
    return {'form': CommentForm(initial=initial_data)}
