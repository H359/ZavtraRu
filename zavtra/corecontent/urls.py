from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url(r'^view/(?P<slug>[-\w]+)/$', 'corecontent.views.viewitem', name='corecontent.view.item'),
)

