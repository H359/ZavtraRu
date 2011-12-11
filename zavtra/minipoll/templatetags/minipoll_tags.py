"""Templatetags for minipoll"""
from django.template import Library
from django.core.exceptions import ObjectDoesNotExist

from minipoll.models import Poll

register = Library()

@register.inclusion_tag('minipoll/tags/poll_form.html')
def display_poll_form(poll):
    return {'poll': poll}

@register.inclusion_tag('minipoll/tags/poll_result.html')
def display_poll_result(poll):
    return {'poll': poll}

@register.inclusion_tag('minipoll/tags/last_poll.html', takes_context=True)
def display_last_poll(context):
    try:
	poll = Poll.published.latest('creation_date')
    except ObjectDoesNotExist:
	return {}
    try:
        session = context['request'].session
        user_has_vote = poll.pk in session.get('poll', [])
    except KeyError:
        user_has_vote = False
    
    return {'poll': poll,
            'user_has_vote': user_has_vote}


