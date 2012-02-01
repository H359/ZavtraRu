"""Urls for minipoll"""
from django.conf.urls.defaults import *

from minipoll.views import poll_list, poll_detail, poll_vote

urlpatterns = patterns('',
   url(r'^$', poll_list, name='minipoll_poll_list'),
)

urlpatterns += patterns('minipoll.views',
    url(r'^(?P<slug>[-\w]+)/$', poll_detail, name='minipoll_poll_detail'),
    url(r'^(?P<slug>[-\w]+)/result/$', poll_detail, {'result': True}, name='minipoll_poll_result'),
    url(r'^(?P<slug>[-\w]+)/vote/$', poll_vote, name='minipoll_poll_vote'),
)

