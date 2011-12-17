from django.conf.urls.defaults import patterns, url

from feeds import LatestContentFeed, RubricContentFeed, TagContentFeed, FeaturedItemsContentFeed

urlpatterns = patterns('',
    url(r'^rubric/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_rubric', name='corecontent.view.rubric'),
    url(r'^news/(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_news', name='corecontent.view.news'),
    url(r'^featured/$', 'corecontent.views.view_featured_index', name='corecontent.view.featured_index'),
    url(r'^featured/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_featured', name='corecontent.view.featured'),
    url(r'^blogs/$', 'corecontent.views.view_blog', name='corecontent.view.blog'),
    url(r'^view/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_item', name='corecontent.view.item'),
    url(r'^tag/(?P<slug>[_\-\w]+)/$', 'corecontent.views.view_items_by_tag', name='corecontent.view.items_by_tag'),
    url(r'^zhivotov/$', 'corecontent.views.zhivotov_gallery', name='corecontent.view.zhivotov_gallery'),
    
    url(r'^rss/latest/$', LatestContentFeed(), name='corecontent.rss.latest'),
    url(r'^rss/rubric/(?P<slug>[_\-\w]+)/$', RubricContentFeed(), name='corecontent.rss.rubric'),
    url(r'^rss/tag/(?P<slug>[_\-\w]+)/$', TagContentFeed(), name='corecontent.rss.tag'),
    url(r'^rss/featured/(?P<slug>[_\-\w]+)/$', FeaturedItemsContentFeed(), name='corecontent.rss.featured'),
)