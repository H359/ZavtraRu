from django.conf.urls.defaults import patterns, url
from django.views.generic.simple import direct_to_template

urlpatterns = patterns('',
    url(r'^register/$', 'siteuser.views.register', name='accounts.register'),
    url(r'^complete/$', 'siteuser.views.complete', name='accounts.complete'),
    url(r'^user/(?P<username>[@.+_\-a-zA-Z0-9]+)/$', 'siteuser.views.user', name='accounts.view.user'),
    url(r'^user/(?P<username>[@.+_\-a-zA-Z0-9]+)/articles/$', 'siteuser.views.user', name='accounts.view.user_articles', kwargs={'part': 'articles'}),
    url(r'^user/(?P<username>[@.+_\-a-zA-Z0-9]+)/comments/$', 'siteuser.views.user', name='accounts.view.user_comments', kwargs={'part': 'comments'}),
)

