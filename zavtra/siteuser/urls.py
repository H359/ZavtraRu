#-*- coding: utf-8 -*-
from django.conf.urls import patterns, url

from siteuser.views import UserView, UsersView


urlpatterns = patterns('',
  url(r'^user/(?P<id>[0-9]+)/$', UserView.as_view(), name='siteuser.views.profile'),
  url(r'^authors/$', UsersView.as_view(), name='siteuser.views.authors'),
  url(ur'^authors/(?P<letter>[еёа-яА-Я]{1})/$', UsersView.as_view(), name='siteuser.views.authors_letter'),
)