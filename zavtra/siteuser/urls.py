#-*- coding: utf-8 -*-
from django.conf.urls import patterns, url

from siteuser.views import ProfileView, ArticlesView, CommentsView,\
                           RegisterView, AuthorsView, SubscribeUser,\
                           UnSubscribeUser, RegisterDoneView


urlpatterns = patterns('',
  url(r'^$', AuthorsView.as_view(), name='siteuser.views.authors'),
  url(r'^user/(?P<pk>[0-9]+)/$', ProfileView.as_view(), name='siteuser.views.profile'),
  url(r'^user/(?P<pk>[0-9]+)/articles/$', ArticlesView.as_view(), name='siteuser.views.profile_articles'),
  url(r'^user/(?P<pk>[0-9]+)/articles/(?P<category>(wod|video|text)+)/$', ArticlesView.as_view()),
  url(r'^user/(?P<pk>[0-9]+)/comments/$', CommentsView.as_view(), name='siteuser.views.profile_comments'),
  url(r'^subscribe/(?P<readee>[0-9]+)/$', SubscribeUser.as_view(), name='siteuser.views.subscribe'),
  url(r'^unsubscribe/(?P<readee>[0-9]+)/$', UnSubscribeUser.as_view(), name='siteuser.views.unsubscribe'),
  url(r'^register/$', RegisterView.as_view(), name='siteuser.views.register'),
  url(r'^register/done/$', RegisterDoneView.as_view(), name='siteuser.views.register_done'),
  url(r'^login/$', 'django.contrib.auth.views.login', {'template_name': 'siteuser/login.jhtml'}, name='siteuser.views.login'),
  url(r'^logout/$', 'django.contrib.auth.views.logout', name='siteuser.views.logout'),
)