from django.conf.urls.defaults import patterns, url

from siteuser.views import UserView


urlpatterns = patterns('',
  url(r'^user/(?P<id>[0-9]+)/$', UserView.as_view(), name='siteuser.views.profile'),
)