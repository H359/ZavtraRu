"""Urls for minipoll"""
from django.conf.urls.defaults import *
from django.views.generic.list_detail import object_list

from minipoll.models import Poll

poll_conf = {'queryset': Poll.published.all(),}

def show_polls(request):
    return object_list(request, queryset=Poll.published.all())

urlpatterns = patterns('',
                       url(r'^$', show_polls, name='minipoll_poll_list'),
                      )

urlpatterns += patterns('minipoll.views',
                        url(r'^(?P<slug>[-\w]+)/$', 'poll_detail',                            
                            name='minipoll_poll_detail'),
                        url(r'^(?P<slug>[-\w]+)/result/$', 'poll_detail',
                            {'template_name': 'minipoll/poll_detail_result.html'},
                            name='minipoll_poll_result'),
                        url(r'^(?P<slug>[-\w]+)/vote/$', 'poll_vote',
                            name='minipoll_poll_vote'),
                        )

