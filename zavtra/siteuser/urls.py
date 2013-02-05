#-*- coding: utf-8 -*-
from django.conf.urls import patterns, url

from siteuser.views import ProfileView, ArticlesView, CommentsView,\
                           CommunityView, CommunityHomeView


urlpatterns = patterns('',
  url(r'^$', CommunityHomeView.as_view(), name='siteuser.views.community'),
  url(r'^(?P<year>\d{4})/(?P<month>\d{1,2})/(?P<day>\d{1,2})/$', CommunityView.as_view()),
  url(r'^user/(?P<pk>[0-9]+)/$', ProfileView.as_view(), name='siteuser.views.profile'),
  url(r'^user/(?P<pk>[0-9]+)/articles/$', ArticlesView.as_view(), name='siteuser.views.profile_articles'),
  url(r'^user/(?P<pk>[0-9]+)/comments/$', CommentsView.as_view(), name='siteuser.views.profile_comments'),
)