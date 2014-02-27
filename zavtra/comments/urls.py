from django.conf.urls import patterns, url
from comments.views import CommentVoteView


urlpatterns = patterns('',
  url(r'^add/$', 'comments.views.add_comment', name='comments.views.add'),
  url(r'^vote/(?P<id>[0-9]+)/(?P<vote>(up|down)+)/$', CommentVoteView.as_view(), name='content.view.comment_vote'),
)