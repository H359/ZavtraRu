from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url(r'^$', 'editorial.views.index', name='editorial.view.index'),
    url(r'^item/(?P<id>[0-9]+)/$', 'editorial.views.thread', name='editorial.view.thread'),
    url(r'^create/$', 'editorial.views.create', name='editorial.view.create_thread'),
)