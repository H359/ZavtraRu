"""Templatetags for minipoll"""
from django.template import Library
from django.core.exceptions import ObjectDoesNotExist
from django.db.models import Count

from utils import cached

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
    poll = cached(
	lambda: Poll.calculate(Poll.objects.filter(status=1).annotate(total_votes=Count('vote')).latest('creation_date')),
	'latest_poll_object',
	duration=60
    )
    try:
        session = context['request'].session
        user_has_vote = poll.pk in session.get('poll', [])
    except KeyError:
        user_has_vote = False
    return {'poll': poll, 'user_has_vote': user_has_vote}