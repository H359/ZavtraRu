from django.conf.urls.defaults import patterns, url
from django.views.generic.simple import direct_to_template

urlpatterns = patterns('',
    url(r'^register/$', 'siteuser.views.register', name='accounts.register'),
    url(r'^complete/$', direct_to_template, {'template': 'registration/registration_complete.html'}, name='accounts.complete'),
)

