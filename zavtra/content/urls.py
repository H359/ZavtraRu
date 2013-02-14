from django.conf.urls import patterns, url

from content.views import ArticleView, RubricView, FeaturedView,\
                          IssueView, ArchiveView, EventsView


urlpatterns = patterns('',
  url(r'^issue/$', 'content.views.current_issue_redirect', name='content.views.current_issue'),
  url(r'^issue/(?P<year>[0-9]+)/(?P<issue>[0-9]+)/$', IssueView.as_view(), name='content.views.zeitung'),
  url(r'^archive/((?P<id>[0-9]+)/){0,1}$', ArchiveView.as_view(), name='content.views.archive'),
  url(r'^view/(?P<slug>[^/]+)/$', ArticleView.as_view(), name='content.views.article'),
  url(r'^rubric/(?P<slug>[^/]+)/$', RubricView.as_view(), name='content.views.rubric'),
  url(r'^events/$', EventsView.as_view(), name='content.views.events'),
  url(r'^events/(?P<date>[0-9]{1,2}-[0-9]{1,2}-[0-9]{4})/$', EventsView.as_view()),
)
