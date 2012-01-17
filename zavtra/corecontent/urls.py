from django.conf.urls.defaults import patterns, url

from feeds import LatestContentFeed, RubricContentFeed, TagContentFeed, FeaturedItemsContentFeed, UnpublishedContentFeed, ExclusiveNewsFeed

urlpatterns = patterns('',
    url(r'^rubric/(?P<slug>[-_A-Za-z0-9]+)/$', 'corecontent.views.view_rubric', name='corecontent.view.rubric'),
    url(r'^news/(?P<year>[0-9]{4})-(?P<month>[0-9]{1,2})-(?P<day>[0-9]{1,2})/(?P<slug>[-_A-Za-z0-9]+)/$', 'corecontent.views.view_news', name='corecontent.view.news'),
    url(r'^featured/$', 'corecontent.views.view_featured_index', name='corecontent.view.featured_index'),
    url(r'^featured/(?P<slug>[-_A-Za-z0-9]+)/$', 'corecontent.views.view_featured', name='corecontent.view.featured'),
    url(r'^blogs/$', 'corecontent.views.view_blog', name='corecontent.view.blog'),
    url(r'^view/(?P<slug>[-_A-Za-z0-9]+)/$', 'corecontent.views.view_item', name='corecontent.view.item'),
    url(r'^tag/(?P<slug>[-_A-Za-z0-9]+)/$', 'corecontent.views.view_items_by_tag', name='corecontent.view.items_by_tag'),
    url(r'^zhivotov/$', 'corecontent.views.zhivotov_gallery', name='corecontent.view.zhivotov_gallery'),
    url(r'^site-only/$', 'corecontent.views.view_unpublished', name='corecontent.view.unpublished_view'),
    url(r'^archive/$', 'corecontent.views.view_archive', name='corecontent.view.archive_view'),
    url(r'^archive/(?P<issue>[0-9]+)/$', 'corecontent.views.view_issue', name='corecontent.view.issue_view'),
    
    url(r'^rss/latest/$', LatestContentFeed(), name='corecontent.rss.latest'),
    url(r'^rss/ynews/$', ExclusiveNewsFeed(), name='corecontent.rss.news_exclusive'),
    url(r'^rss/rubric/(?P<slug>[-_A-Za-z0-9]+)/$', RubricContentFeed(), name='corecontent.rss.rubric'),
    url(r'^rss/tag/(?P<slug>[-_A-Za-z0-9]+)/$', TagContentFeed(), name='corecontent.rss.tag'),
    url(r'^rss/featured/(?P<slug>[-_A-Za-z0-9]+)/$', FeaturedItemsContentFeed(), name='corecontent.rss.featured'),
    url(r'^rss/site-only/$', UnpublishedContentFeed(), name='corecontent.rss.unpublished'),
)