from django.conf.urls.defaults import patterns, url

urlpatterns = patterns('',
    url(r'^rubric/(?P<slug>[-\w]+)/$', 'corecontent.views.view_rubric', name='corecontent.view.rubric'),
    url(r'^featured/(?P<slug>[-\w]+)/$', 'corecontent.views.view_featured', name='corecontent.view.featured'),
    url(r'^view/(?P<slug>[-\w]+)/$', 'corecontent.views.view_item', name='corecontent.view.item'),
    url(r'^tag/(?P<slug>[-\w]+)/$', 'corecontent.views.view_items_by_tag', name = 'corecontent.view.items_by_tag')
)

