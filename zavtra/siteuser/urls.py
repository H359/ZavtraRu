from django.conf.urls.defaults import patterns, url
from django.views.generic.simple import direct_to_template

urlpatterns = patterns('',
    url(r'^register/$', 'siteuser.views.register', name='accounts.register'),
    url(r'^complete/$', direct_to_template, {'template': 'registration/registration_complete.html'}, name='accounts.complete'),
    url(r'^user/(?P<username>(\W+))/$', 'siteuser.views.user', name='accounts.view.user'),
    url(r'^user/(?P<username>(\W+))/articles/$', 'siteuser.views.user', name='accounts.view.user_articles', kwargs={'part': 'articles'}),
    url(r'^user/(?P<username>(\W+))/comments/$', 'siteuser.views.user', name='accounts.view.user_comments', kwargs={'part': 'comments'}),
)

