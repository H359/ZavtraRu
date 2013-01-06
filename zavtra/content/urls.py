from django.conf.urls.defaults import patterns, url

from content.views import ArticleView, RubricView, FeaturedView, ZeitungView


urlpatterns = patterns('',
  url(r'^zeitung/(?P<year>[0-9]+)/(?P<issue>[0-9]+)/$', ZeitungView.as_view(), name='content.views.zeitung'),
  url(r'^view/(?P<slug>[^/]+)/$', ArticleView.as_view(), name='content.views.article'),
  url(r'^rubric/(?P<slug>[^/]+)/$', RubricView.as_view(), name='content.views.rubric'),
  url(r'^featured/(?P<slug>[^/]+)/$', FeaturedView.as_view(), name='content.views.topic')
)