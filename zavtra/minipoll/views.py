"""Views for minipoll"""
from django.shortcuts import redirect
from django.shortcuts import get_object_or_404
from django.views.generic.list_detail import object_detail

from minipoll.models import Poll
from minipoll.models import Choice
from minipoll.models import Vote

def poll_detail(request, slug, template_name='minipoll/poll_detail.html'):
    """Detail for a poll"""
    poll = get_object_or_404(Poll, slug=slug)
    user_has_vote = poll.pk in request.session.get('poll', [])
    
    return object_detail(request, slug=slug,
                         queryset=Poll.batched.batch_select('choices').all(),
                         template_name=template_name,
                         extra_context={'user_has_vote': user_has_vote})

def poll_vote(request, slug):
    """Vote for a poll"""
    poll = get_object_or_404(Poll, slug=slug)
    
    if request.POST.get('choice'):
        choice_id = request.POST['choice']
        choice = get_object_or_404(Choice, pk=choice_id, poll=poll)

        vote = Vote(poll=poll, choice=choice, ip_address=request.META['REMOTE_ADDR'])
        if request.user.is_authenticated():
            vote.voter = request.user
        vote.save()

        request.session.setdefault('poll', [])
        request.session['poll'].append(poll.pk)
        request.session.modified = True

        return redirect('minipoll_poll_result', slug=poll.slug)

    return redirect(poll)
