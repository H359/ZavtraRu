from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url(r'^rubric/(?P<slug>[-\w]+)/$', 'corecontent.views.view_rubric', name='corecontent.view.rubric'),
    url(r'^view/(?P<slug>[-\w]+)/$', 'corecontent.views.view_item', name='corecontent.view.item'),
)

