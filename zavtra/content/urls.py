from django.conf.urls import patterns, url

from content.views import ArticleView, RubricView, IssueView,\
                          ArchiveView, EventsView, TopicView,\
                          CommunityView, ArticleVoteView, SearchView


urlpatterns = patterns('',
  url(r'^issue/$', 'content.views.current_issue_redirect', name='content.view.current_issue'),
  url(r'^issue/(?P<year>[0-9]+)/(?P<issue>[0-9]+)/$', IssueView.as_view(), name='content.view.zeitung'),
  url(r'^archive/$', ArchiveView.as_view(), name='content.view.archive'),
  url(r'^view/(?P<slug>[^/]+)/$', ArticleView.as_view(), name='content.view.article'),
  url(r'^vote/(?P<slug>[^/]+)/(?P<vote>(up|down)+)/$', ArticleVoteView.as_view(), name='content.view.article_vote'),
  url(r'^rubric/(?P<slug>[^/]+)/$', RubricView.as_view(), name='content.view.rubric'),
  url(r'^events/$', EventsView.as_view(), name='content.view.events'),
  url(r'^events/(?P<date>[0-9]{1,2}-[0-9]{1,2}-[0-9]{4})/$', EventsView.as_view()),
  url(r'^topic/(?P<slug>[^/]+)/$', TopicView.as_view(), name='content.view.topic'),
  url(r'^community/$', CommunityView.as_view(), name='content.view.community'),
  url(r'^search/$', SearchView.as_view(), name='content.view.search'),
  url(r'^search/(?P<category>(articles|wod|events))/$', SearchView.as_view())
)
