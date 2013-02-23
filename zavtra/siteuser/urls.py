#-*- coding: utf-8 -*-
from django.conf.urls import patterns, url
from django.contrib.auth.decorators import login_required

from siteuser.views import AuthorsView,\
                           ProfileView, ProfileArticlesView, ProfileCommentsView,\
                           RegisterView, RegisterDoneView,\
                           SubscribeUserView, UnSubscribeUserView,\
                           CabinetView, CabinetArticlesView,\
                           CabinetArticlesSubscriptionsView,\
                           CabinetAuthorsSubscriptionsView


urlpatterns = patterns('',
  url(r'^$', AuthorsView.as_view(), name='siteuser.view.authors'),
  url(r'^user/(?P<pk>[0-9]+)/$', ProfileView.as_view(), name='siteuser.view.profile'),
  url(r'^user/(?P<pk>[0-9]+)/articles/$', ProfileArticlesView.as_view(), name='siteuser.view.profile_articles'),
  url(r'^user/(?P<pk>[0-9]+)/articles/(?P<category>(wod|video|text))/$', ProfileArticlesView.as_view(), name='siteuser.view.profile_articles_category'),
  url(r'^user/(?P<pk>[0-9]+)/comments/$', ProfileCommentsView.as_view(), name='siteuser.view.profile_comments'),
  url(r'^subscribe/(?P<readee>[0-9]+)/$', SubscribeUserView.as_view(), name='siteuser.view.subscribe'),
  url(r'^unsubscribe/(?P<readee>[0-9]+)/$', UnSubscribeUserView.as_view(), name='siteuser.view.unsubscribe'),
  url(r'^cabinet/$', login_required(CabinetView.as_view()), name='siteuser.view.cabinet'),
  url(r'^cabinet/subscriptions/articles/$', login_required(CabinetArticlesSubscriptionsView.as_view()), name='siteuser.view.cabinet_subscriptions_articles'),
  url(r'^cabinet/subscriptions/articles/(?P<category>(wod|video|text))/$', login_required(CabinetArticlesSubscriptionsView.as_view()), name='siteuser.view.cabinet_subscriptions_articles_category'),
  url(r'^cabinet/subscriptions/authors/$', login_required(CabinetAuthorsSubscriptionsView.as_view()), name='siteuser.view.cabinet_subscriptions_authors'),
  url(r'^cabinet/articles/$', login_required(CabinetArticlesView.as_view()), name='siteuser.view.cabinet_articles'),
  url(r'^cabinet/articles/(?P<category>(wod|video|text)+)/$', login_required(CabinetArticlesView.as_view()), name='siteuser.view.cabinet_articles_category'),
  url(r'^register/$', RegisterView.as_view(), name='siteuser.view.register'),
  url(r'^register/done/$', RegisterDoneView.as_view(), name='siteuser.view.register_done'),
  url(r'^login/$', 'django.contrib.auth.views.login', {'template_name': 'siteuser/login.jhtml'}, name='siteuser.view.login'),
  url(r'^logout/$', 'django.contrib.auth.views.logout', name='siteuser.view.logout'),
)