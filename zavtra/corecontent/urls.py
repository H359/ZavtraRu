from django.conf.urls.defaults import patterns, url

from feeds import LatestContentFeed

urlpatterns = patterns('',
    url(r'^rubric/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_rubric', name='corecontent.view.rubric'),
    url(r'^featured/$', 'corecontent.views.view_featured_index', name='corecontent.view.featured_index'),
    url(r'^featured/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_featured', name='corecontent.view.featured'),
    url(r'^blogs/$', 'corecontent.views.view_blog', name='corecontent.view.blog'),
    url(r'^view/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_item', name='corecontent.view.item'),
    url(r'^tag/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_items_by_tag', name='corecontent.view.items_by_tag'),
    url(r'^zhivotov/$', 'corecontent.views.zhivotov_gallery', name='corecontent.view.zhivotov_gallery'),
    
    url(r'^rss/latest/$', LatestContentFeed(), name='corecontent.rss.latest'),
)