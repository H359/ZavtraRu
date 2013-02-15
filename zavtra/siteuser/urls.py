#-*- coding: utf-8 -*-
from django.conf.urls import patterns, url

from siteuser.views import ProfileView, ArticlesView, CommentsView,\
                           CommunityView, RegisterView


urlpatterns = patterns('',
  url(r'^$', CommunityView.as_view(), name='siteuser.views.community'),
  url(r'^user/(?P<pk>[0-9]+)/$', ProfileView.as_view(), name='siteuser.views.profile'),
  url(r'^user/(?P<pk>[0-9]+)/articles/$', ArticlesView.as_view(), name='siteuser.views.profile_articles'),
  url(r'^user/(?P<pk>[0-9]+)/comments/$', CommentsView.as_view(), name='siteuser.views.profile_comments'),
  url(r'^register/$', RegisterView.as_view(), name='siteuser.views.register'),
)