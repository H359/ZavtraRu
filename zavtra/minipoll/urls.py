"""Urls for minipoll"""
from django.conf.urls.defaults import *
from django.views.generic.list_detail import object_list
from django.db.models import Count

from minipoll.models import Poll

poll_conf = {'queryset': Poll.objects.filter(status=1).all(),}

def show_polls(request):
    return object_list(request, queryset=Poll.objects.annotate(total_votes=Count('vote')).filter(status=1).all())

urlpatterns = patterns('',
   url(r'^$', show_polls, name='minipoll_poll_list'),
)

urlpatterns += patterns('minipoll.views',
    url(r'^(?P<slug>[-\w]+)/$', 'poll_detail', name='minipoll_poll_detail'),
    url(r'^(?P<slug>[-\w]+)/result/$', 'poll_detail', {'detail': True}, name='minipoll_poll_result'),
    url(r'^(?P<slug>[-\w]+)/vote/$', 'poll_vote', name='minipoll_poll_vote'),
)

