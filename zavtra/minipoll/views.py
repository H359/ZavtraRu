"""Views for minipoll"""
from django.shortcuts import redirect, Http404, get_object_or_404
from django.views.generic import DetailView, ListView
from django.db.models import Count

from utils import MakoViewMixin
from diggpaginator import DiggPaginator

from annoying.decorators import render_to

from minipoll.models import Poll
from minipoll.models import Choice
from minipoll.models import Vote

class PollDetail(MakoViewMixin, DetailView):
    template_name = 'minipoll/poll_detail.html'
    context_object_name = 'poll'
    def get_context_data(self, **kwargs):
	context = super(PollDetail, self).get_context_data(**kwargs)
	context['force_result'] = self.kwargs.get('result', False)
	return context
    def get_object(self):
	return Poll.calculate( Poll.objects.annotate(total_votes=Count('vote')).get(slug=self.kwargs['slug']) )

class PollList(MakoViewMixin, ListView):
    paginate_by = 15
    paginator_class = DiggPaginator
    template_name = 'minipoll/poll_list.html'
    context_object_name = 'polls'
    def get_queryset(self):
	return Poll.objects.annotate(total_votes=Count('vote')).filter(status=1)

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

poll_detail = PollDetail.as_view()
poll_list = PollList.as_view()