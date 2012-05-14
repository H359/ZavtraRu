from django.http import HttpResponseNotAllowed
from django.shortcuts import get_object_or_404, redirect

from models import Poll, Option, Answer

def poll(request, poll_id):
    if request.method != 'POST':
	return HttpResponseNotAllowed()
    poll = get_object_or_404(Poll, pk=poll_id)
    payload_key = u'poll.%d' % poll.pk
    options = Option.objects.filter(id__in = request.POST.getlist(payload_key))
    for option in options:
	Answer.objects.create(poll=poll, user=request.user, chosen=option)
    return redirect(poll.content_object.get_absolute_url())