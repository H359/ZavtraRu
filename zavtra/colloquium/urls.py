from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url('^poll/(?P<poll_id>[0-9]+)/$', 'colloquium.views.poll', name='colloquium.poll'),
)