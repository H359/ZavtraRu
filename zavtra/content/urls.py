from django.conf.urls.defaults import patterns, url

from content.views import ArticleView, RubricView


urlpatterns = patterns('',
  url(r'^view/(?P<slug>[^/]+)/$', ArticleView.as_view(), name='content.views.article'),
  url(r'^rubric/(?P<slug>[^/]+)/$', RubricView.as_view(), name='content.views.rubric'),
)